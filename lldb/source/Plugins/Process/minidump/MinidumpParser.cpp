//===-- MinidumpParser.cpp ---------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MinidumpParser.h"
#include "NtStructures.h"
#include "RegisterContextMinidump_x86_32.h"

#include "lldb/Utility/LLDBAssert.h"
#include "Plugins/Process/Utility/LinuxProcMaps.h"

// C includes
// C++ includes
#include <algorithm>
#include <map>
#include <vector>
#include <utility>

using namespace lldb_private;
using namespace minidump;

static llvm::Error stringError(llvm::StringRef Err) {
  return llvm::make_error<llvm::StringError>(Err,
                                             llvm::inconvertibleErrorCode());
}

llvm::Expected<MinidumpParser>
MinidumpParser::Create(const lldb::DataBufferSP &data_sp) {
  if (data_sp->GetByteSize() < sizeof(MinidumpHeader))
    return stringError("Buffer too small.");

  llvm::ArrayRef<uint8_t> header_data(data_sp->GetBytes(),
                                      sizeof(MinidumpHeader));
  const MinidumpHeader *header = MinidumpHeader::Parse(header_data);
  if (!header)
    return stringError("invalid minidump: can't parse the header");

  // A minidump without at least one stream is clearly ill-formed
  if (header->streams_count == 0)
    return stringError("invalid minidump: no streams present");

  struct FileRange {
    uint32_t offset = 0;
    uint32_t size = 0;

    FileRange(uint32_t offset, uint32_t size) : offset(offset), size(size) {}
    uint32_t end() const { return offset + size; }
  };

  const uint32_t file_size = data_sp->GetByteSize();

  // Build a global minidump file map, checking for:
  // - overlapping streams/data structures
  // - truncation (streams pointing past the end of file)
  std::vector<FileRange> minidump_map;

  minidump_map.emplace_back(0, sizeof(MinidumpHeader));

  // Add the directory entries to the file map
  FileRange directory_range(header->stream_directory_rva,
                            header->streams_count * sizeof(MinidumpDirectory));
  if (directory_range.end() > file_size)
    return stringError("invalid minidump: truncated streams directory");
  minidump_map.push_back(directory_range);

  llvm::DenseMap<uint32_t, MinidumpLocationDescriptor> directory_map;

  // Parse stream directory entries
  llvm::ArrayRef<uint8_t> directory_data(
      data_sp->GetBytes() + directory_range.offset, directory_range.size);
  for (uint32_t i = 0; i < header->streams_count; ++i) {
    const MinidumpDirectory *directory_entry = nullptr;
    Status error = consumeObject(directory_data, directory_entry);
    if (error.Fail())
      return error.ToError();
    if (directory_entry->stream_type == 0) {
      // Ignore dummy streams (technically ill-formed, but a number of
      // existing minidumps seem to contain such streams)
      if (directory_entry->location.data_size == 0)
        continue;
      return stringError("invalid minidump: bad stream type");
    }
    // Update the streams map, checking for duplicate stream types
    if (!directory_map
             .insert({directory_entry->stream_type, directory_entry->location})
             .second)
      return stringError("invalid minidump: duplicate stream type");

    // Ignore the zero-length streams for layout checks
    if (directory_entry->location.data_size != 0) {
      minidump_map.emplace_back(directory_entry->location.rva,
                                directory_entry->location.data_size);
    }
  }

  // Sort the file map ranges by start offset
  llvm::sort(minidump_map.begin(), minidump_map.end(),
             [](const FileRange &a, const FileRange &b) {
               return a.offset < b.offset;
             });

  // Check for overlapping streams/data structures
  for (size_t i = 1; i < minidump_map.size(); ++i) {
    const auto &prev_range = minidump_map[i - 1];
    if (prev_range.end() > minidump_map[i].offset)
      return stringError("invalid minidump: overlapping streams");
  }

  // Check for streams past the end of file
  const auto &last_range = minidump_map.back();
  if (last_range.end() > file_size)
    return stringError("invalid minidump: truncated stream");

  return MinidumpParser(std::move(data_sp), std::move(directory_map));
}

MinidumpParser::MinidumpParser(
    lldb::DataBufferSP data_sp,
    llvm::DenseMap<uint32_t, MinidumpLocationDescriptor> directory_map)
    : m_data_sp(std::move(data_sp)), m_directory_map(std::move(directory_map)) {
}

llvm::ArrayRef<uint8_t> MinidumpParser::GetData() {
  return llvm::ArrayRef<uint8_t>(m_data_sp->GetBytes(),
                                 m_data_sp->GetByteSize());
}

llvm::ArrayRef<uint8_t>
MinidumpParser::GetStream(StreamType stream_type) {
  auto iter = m_directory_map.find(static_cast<uint32_t>(stream_type));
  if (iter == m_directory_map.end())
    return {};

  // check if there is enough data
  if (iter->second.rva + iter->second.data_size > m_data_sp->GetByteSize())
    return {};

  return llvm::ArrayRef<uint8_t>(m_data_sp->GetBytes() + iter->second.rva,
                                 iter->second.data_size);
}

llvm::Optional<std::string> MinidumpParser::GetMinidumpString(uint32_t rva) {
  auto arr_ref = m_data_sp->GetData();
  if (rva > arr_ref.size())
    return llvm::None;
  arr_ref = arr_ref.drop_front(rva);
  return parseMinidumpString(arr_ref);
}

UUID MinidumpParser::GetModuleUUID(const MinidumpModule *module) {
  auto cv_record =
      GetData().slice(module->CV_record.rva, module->CV_record.data_size);

  // Read the CV record signature
  const llvm::support::ulittle32_t *signature = nullptr;
  Status error = consumeObject(cv_record, signature);
  if (error.Fail())
    return UUID();

  const CvSignature cv_signature =
      static_cast<CvSignature>(static_cast<uint32_t>(*signature));

  if (cv_signature == CvSignature::Pdb70) {
    const CvRecordPdb70 *pdb70_uuid = nullptr;
    Status error = consumeObject(cv_record, pdb70_uuid);
    if (error.Fail())
      return UUID();
    // If the age field is not zero, then include the entire pdb70_uuid struct
    if (pdb70_uuid->Age != 0)
      return UUID::fromData(pdb70_uuid, sizeof(*pdb70_uuid));

    // Many times UUIDs are all zeroes. This can cause more than one module
    // to claim it has a valid UUID of all zeroes and causes the files to all
    // merge into the first module that claims this valid zero UUID.
    bool all_zeroes = true;
    for (size_t i = 0; all_zeroes && i < sizeof(pdb70_uuid->Uuid); ++i)
      all_zeroes = pdb70_uuid->Uuid[i] == 0;
    if (all_zeroes)
      return UUID();
    
    if (GetArchitecture().GetTriple().getVendor() == llvm::Triple::Apple) {
      // Breakpad incorrectly byte swaps the first 32 bit and next 2 16 bit
      // values in the UUID field. Undo this so we can match things up
      // with our symbol files
      uint8_t apple_uuid[16];
      // Byte swap the first 32 bits
      apple_uuid[0] = pdb70_uuid->Uuid[3];
      apple_uuid[1] = pdb70_uuid->Uuid[2];
      apple_uuid[2] = pdb70_uuid->Uuid[1];
      apple_uuid[3] = pdb70_uuid->Uuid[0];
      // Byte swap the next 16 bit value
      apple_uuid[4] = pdb70_uuid->Uuid[5];
      apple_uuid[5] = pdb70_uuid->Uuid[4];
      // Byte swap the next 16 bit value
      apple_uuid[6] = pdb70_uuid->Uuid[7];
      apple_uuid[7] = pdb70_uuid->Uuid[6];
      for (size_t i = 8; i < sizeof(pdb70_uuid->Uuid); ++i)
        apple_uuid[i] = pdb70_uuid->Uuid[i];
      return UUID::fromData(apple_uuid, sizeof(apple_uuid));
    }
    return UUID::fromData(pdb70_uuid->Uuid, sizeof(pdb70_uuid->Uuid));
  } else if (cv_signature == CvSignature::ElfBuildId)
    return UUID::fromOptionalData(cv_record);

  return UUID();
}

llvm::ArrayRef<MinidumpThread> MinidumpParser::GetThreads() {
  llvm::ArrayRef<uint8_t> data = GetStream(StreamType::ThreadList);

  if (data.size() == 0)
    return llvm::None;

  return MinidumpThread::ParseThreadList(data);
}

llvm::ArrayRef<uint8_t>
MinidumpParser::GetThreadContext(const MinidumpLocationDescriptor &location) {
  if (location.rva + location.data_size > GetData().size())
    return {};
  return GetData().slice(location.rva, location.data_size);
}

llvm::ArrayRef<uint8_t>
MinidumpParser::GetThreadContext(const MinidumpThread &td) {
  return GetThreadContext(td.thread_context);
}

llvm::ArrayRef<uint8_t>
MinidumpParser::GetThreadContextWow64(const MinidumpThread &td) {
  // On Windows, a 32-bit process can run on a 64-bit machine under WOW64. If
  // the minidump was captured with a 64-bit debugger, then the CONTEXT we just
  // grabbed from the mini_dump_thread is the one for the 64-bit "native"
  // process rather than the 32-bit "guest" process we care about.  In this
  // case, we can get the 32-bit CONTEXT from the TEB (Thread Environment
  // Block) of the 64-bit process.
  auto teb_mem = GetMemory(td.teb, sizeof(TEB64));
  if (teb_mem.empty())
    return {};

  const TEB64 *wow64teb;
  Status error = consumeObject(teb_mem, wow64teb);
  if (error.Fail())
    return {};

  // Slot 1 of the thread-local storage in the 64-bit TEB points to a structure
  // that includes the 32-bit CONTEXT (after a ULONG). See:
  // https://msdn.microsoft.com/en-us/library/ms681670.aspx
  auto context =
      GetMemory(wow64teb->tls_slots[1] + 4, sizeof(MinidumpContext_x86_32));
  if (context.size() < sizeof(MinidumpContext_x86_32))
    return {};

  return context;
  // NOTE:  We don't currently use the TEB for anything else.  If we
  // need it in the future, the 32-bit TEB is located according to the address
  // stored in the first slot of the 64-bit TEB (wow64teb.Reserved1[0]).
}

const MinidumpSystemInfo *MinidumpParser::GetSystemInfo() {
  llvm::ArrayRef<uint8_t> data = GetStream(StreamType::SystemInfo);

  if (data.size() == 0)
    return nullptr;

  return MinidumpSystemInfo::Parse(data);
}

ArchSpec MinidumpParser::GetArchitecture() {
  if (m_arch.IsValid())
    return m_arch;

  // Set the architecture in m_arch
  const MinidumpSystemInfo *system_info = GetSystemInfo();

  if (!system_info)
    return m_arch;

  // TODO what to do about big endiand flavors of arm ?
  // TODO set the arm subarch stuff if the minidump has info about it

  llvm::Triple triple;
  triple.setVendor(llvm::Triple::VendorType::UnknownVendor);

  auto arch = static_cast<ProcessorArchitecture>(
      static_cast<uint32_t>(system_info->processor_arch));

  switch (arch) {
  case ProcessorArchitecture::X86:
    triple.setArch(llvm::Triple::ArchType::x86);
    break;
  case ProcessorArchitecture::AMD64:
    triple.setArch(llvm::Triple::ArchType::x86_64);
    break;
  case ProcessorArchitecture::ARM:
    triple.setArch(llvm::Triple::ArchType::arm);
    break;
  case ProcessorArchitecture::ARM64:
    triple.setArch(llvm::Triple::ArchType::aarch64);
    break;
  default:
    triple.setArch(llvm::Triple::ArchType::UnknownArch);
    break;
  }

  auto os =
      static_cast<OSPlatform>(static_cast<uint32_t>(system_info->platform_id));

  // TODO add all of the OSes that Minidump/breakpad distinguishes?
  switch (os) {
  case OSPlatform::Win32S:
  case OSPlatform::Win32Windows:
  case OSPlatform::Win32NT:
  case OSPlatform::Win32CE:
    triple.setOS(llvm::Triple::OSType::Win32);
    break;
  case OSPlatform::Linux:
    triple.setOS(llvm::Triple::OSType::Linux);
    break;
  case OSPlatform::MacOSX:
    triple.setOS(llvm::Triple::OSType::MacOSX);
    triple.setVendor(llvm::Triple::Apple);
    break;
  case OSPlatform::IOS:
    triple.setOS(llvm::Triple::OSType::IOS);
    triple.setVendor(llvm::Triple::Apple);
    break;
  case OSPlatform::Android:
    triple.setOS(llvm::Triple::OSType::Linux);
    triple.setEnvironment(llvm::Triple::EnvironmentType::Android);
    break;
  default: {
    triple.setOS(llvm::Triple::OSType::UnknownOS);
    std::string csd_version;
    if (auto s = GetMinidumpString(system_info->csd_version_rva))
      csd_version = *s;
    if (csd_version.find("Linux") != std::string::npos)
      triple.setOS(llvm::Triple::OSType::Linux);
    break;
    }
  }
  m_arch.SetTriple(triple);
  return m_arch;
}

const MinidumpMiscInfo *MinidumpParser::GetMiscInfo() {
  llvm::ArrayRef<uint8_t> data = GetStream(StreamType::MiscInfo);

  if (data.size() == 0)
    return nullptr;

  return MinidumpMiscInfo::Parse(data);
}

llvm::Optional<LinuxProcStatus> MinidumpParser::GetLinuxProcStatus() {
  llvm::ArrayRef<uint8_t> data = GetStream(StreamType::LinuxProcStatus);

  if (data.size() == 0)
    return llvm::None;

  return LinuxProcStatus::Parse(data);
}

llvm::Optional<lldb::pid_t> MinidumpParser::GetPid() {
  const MinidumpMiscInfo *misc_info = GetMiscInfo();
  if (misc_info != nullptr) {
    return misc_info->GetPid();
  }

  llvm::Optional<LinuxProcStatus> proc_status = GetLinuxProcStatus();
  if (proc_status.hasValue()) {
    return proc_status->GetPid();
  }

  return llvm::None;
}

llvm::ArrayRef<MinidumpModule> MinidumpParser::GetModuleList() {
  llvm::ArrayRef<uint8_t> data = GetStream(StreamType::ModuleList);

  if (data.size() == 0)
    return {};

  return MinidumpModule::ParseModuleList(data);
}

std::vector<const MinidumpModule *> MinidumpParser::GetFilteredModuleList() {
  llvm::ArrayRef<MinidumpModule> modules = GetModuleList();
  // map module_name -> filtered_modules index
  typedef llvm::StringMap<size_t> MapType;
  MapType module_name_to_filtered_index;

  std::vector<const MinidumpModule *> filtered_modules;
  
  llvm::Optional<std::string> name;
  std::string module_name;

  for (const auto &module : modules) {
    name = GetMinidumpString(module.module_name_rva);
    
    if (!name)
      continue;
    
    module_name = name.getValue();
    
    MapType::iterator iter;
    bool inserted;
    // See if we have inserted this module aready into filtered_modules. If we
    // haven't insert an entry into module_name_to_filtered_index with the
    // index where we will insert it if it isn't in the vector already.
    std::tie(iter, inserted) = module_name_to_filtered_index.try_emplace(
        module_name, filtered_modules.size());

    if (inserted) {
      // This module has not been seen yet, insert it into filtered_modules at
      // the index that was inserted into module_name_to_filtered_index using
      // "filtered_modules.size()" above.
      filtered_modules.push_back(&module);
    } else {
      // This module has been seen. Modules are sometimes mentioned multiple
      // times when they are mapped discontiguously, so find the module with
      // the lowest "base_of_image" and use that as the filtered module.
      auto dup_module = filtered_modules[iter->second];
      if (module.base_of_image < dup_module->base_of_image)
        filtered_modules[iter->second] = &module;
    }
  }
  return filtered_modules;
}

const MinidumpExceptionStream *MinidumpParser::GetExceptionStream() {
  llvm::ArrayRef<uint8_t> data = GetStream(StreamType::Exception);

  if (data.size() == 0)
    return nullptr;

  return MinidumpExceptionStream::Parse(data);
}

llvm::Optional<minidump::Range>
MinidumpParser::FindMemoryRange(lldb::addr_t addr) {
  llvm::ArrayRef<uint8_t> data = GetStream(StreamType::MemoryList);
  llvm::ArrayRef<uint8_t> data64 = GetStream(StreamType::Memory64List);

  if (data.empty() && data64.empty())
    return llvm::None;

  if (!data.empty()) {
    llvm::ArrayRef<MinidumpMemoryDescriptor> memory_list =
        MinidumpMemoryDescriptor::ParseMemoryList(data);

    if (memory_list.empty())
      return llvm::None;

    for (const auto &memory_desc : memory_list) {
      const MinidumpLocationDescriptor &loc_desc = memory_desc.memory;
      const lldb::addr_t range_start = memory_desc.start_of_memory_range;
      const size_t range_size = loc_desc.data_size;

      if (loc_desc.rva + loc_desc.data_size > GetData().size())
        return llvm::None;

      if (range_start <= addr && addr < range_start + range_size) {
        return minidump::Range(range_start,
                               GetData().slice(loc_desc.rva, range_size));
      }
    }
  }

  // Some Minidumps have a Memory64ListStream that captures all the heap memory
  // (full-memory Minidumps).  We can't exactly use the same loop as above,
  // because the Minidump uses slightly different data structures to describe
  // those

  if (!data64.empty()) {
    llvm::ArrayRef<MinidumpMemoryDescriptor64> memory64_list;
    uint64_t base_rva;
    std::tie(memory64_list, base_rva) =
        MinidumpMemoryDescriptor64::ParseMemory64List(data64);

    if (memory64_list.empty())
      return llvm::None;

    for (const auto &memory_desc64 : memory64_list) {
      const lldb::addr_t range_start = memory_desc64.start_of_memory_range;
      const size_t range_size = memory_desc64.data_size;

      if (base_rva + range_size > GetData().size())
        return llvm::None;

      if (range_start <= addr && addr < range_start + range_size) {
        return minidump::Range(range_start,
                               GetData().slice(base_rva, range_size));
      }
      base_rva += range_size;
    }
  }

  return llvm::None;
}

llvm::ArrayRef<uint8_t> MinidumpParser::GetMemory(lldb::addr_t addr,
                                                  size_t size) {
  // I don't have a sense of how frequently this is called or how many memory
  // ranges a Minidump typically has, so I'm not sure if searching for the
  // appropriate range linearly each time is stupid.  Perhaps we should build
  // an index for faster lookups.
  llvm::Optional<minidump::Range> range = FindMemoryRange(addr);
  if (!range)
    return {};

  // There's at least some overlap between the beginning of the desired range
  // (addr) and the current range.  Figure out where the overlap begins and how
  // much overlap there is.

  const size_t offset = addr - range->start;

  if (addr < range->start || offset >= range->range_ref.size())
    return {};

  const size_t overlap = std::min(size, range->range_ref.size() - offset);
  return range->range_ref.slice(offset, overlap);
}

static bool
CreateRegionsCacheFromLinuxMaps(MinidumpParser &parser,
                                std::vector<MemoryRegionInfo> &regions) {
  auto data = parser.GetStream(StreamType::LinuxMaps);
  if (data.empty())
    return false;
  ParseLinuxMapRegions(llvm::toStringRef(data),
                       [&](const lldb_private::MemoryRegionInfo &region,
                           const lldb_private::Status &status) -> bool {
    if (status.Success())
      regions.push_back(region);
    return true;
  });
  return !regions.empty();
}

static bool
CreateRegionsCacheFromMemoryInfoList(MinidumpParser &parser,
                                     std::vector<MemoryRegionInfo> &regions) {
  auto data = parser.GetStream(StreamType::MemoryInfoList);
  if (data.empty())
    return false;
  auto mem_info_list = MinidumpMemoryInfo::ParseMemoryInfoList(data);
  if (mem_info_list.empty())
    return false;
  constexpr auto yes = MemoryRegionInfo::eYes;
  constexpr auto no = MemoryRegionInfo::eNo;
  regions.reserve(mem_info_list.size());
  for (const auto &entry : mem_info_list) {
    MemoryRegionInfo region;
    region.GetRange().SetRangeBase(entry->base_address);
    region.GetRange().SetByteSize(entry->region_size);
    region.SetReadable(entry->isReadable() ? yes : no);
    region.SetWritable(entry->isWritable() ? yes : no);
    region.SetExecutable(entry->isExecutable() ? yes : no);
    region.SetMapped(entry->isMapped() ? yes : no);
    regions.push_back(region);
  }
  return !regions.empty();
}

static bool
CreateRegionsCacheFromMemoryList(MinidumpParser &parser,
                                 std::vector<MemoryRegionInfo> &regions) {
  auto data = parser.GetStream(StreamType::MemoryList);
  if (data.empty())
    return false;
  auto memory_list = MinidumpMemoryDescriptor::ParseMemoryList(data);
  if (memory_list.empty())
    return false;
  regions.reserve(memory_list.size());
  for (const auto &memory_desc : memory_list) {
    if (memory_desc.memory.data_size == 0)
      continue;
    MemoryRegionInfo region;
    region.GetRange().SetRangeBase(memory_desc.start_of_memory_range);
    region.GetRange().SetByteSize(memory_desc.memory.data_size);
    region.SetReadable(MemoryRegionInfo::eYes);
    region.SetMapped(MemoryRegionInfo::eYes);
    regions.push_back(region);
  }
  regions.shrink_to_fit();
  return !regions.empty();
}

static bool
CreateRegionsCacheFromMemory64List(MinidumpParser &parser,
                                   std::vector<MemoryRegionInfo> &regions) {
  llvm::ArrayRef<uint8_t> data =
      parser.GetStream(StreamType::Memory64List);
  if (data.empty())
    return false;
  llvm::ArrayRef<MinidumpMemoryDescriptor64> memory64_list;
  uint64_t base_rva;
  std::tie(memory64_list, base_rva) =
      MinidumpMemoryDescriptor64::ParseMemory64List(data);
  
  if (memory64_list.empty())
    return false;
    
  regions.reserve(memory64_list.size());
  for (const auto &memory_desc : memory64_list) {
    if (memory_desc.data_size == 0)
      continue;
    MemoryRegionInfo region;
    region.GetRange().SetRangeBase(memory_desc.start_of_memory_range);
    region.GetRange().SetByteSize(memory_desc.data_size);
    region.SetReadable(MemoryRegionInfo::eYes);
    region.SetMapped(MemoryRegionInfo::eYes);
    regions.push_back(region);
  }
  regions.shrink_to_fit();
  return !regions.empty();
}

MemoryRegionInfo
MinidumpParser::FindMemoryRegion(lldb::addr_t load_addr) const {
  auto begin = m_regions.begin();
  auto end = m_regions.end();
  auto pos = std::lower_bound(begin, end, load_addr);
  if (pos != end && pos->GetRange().Contains(load_addr))
    return *pos;
  
  MemoryRegionInfo region;
  if (pos == begin)
    region.GetRange().SetRangeBase(0);
  else {
    auto prev = pos - 1;
    if (prev->GetRange().Contains(load_addr))
      return *prev;
    region.GetRange().SetRangeBase(prev->GetRange().GetRangeEnd());
  }
  if (pos == end)
    region.GetRange().SetRangeEnd(UINT64_MAX);
  else
    region.GetRange().SetRangeEnd(pos->GetRange().GetRangeBase());
  region.SetReadable(MemoryRegionInfo::eNo);
  region.SetWritable(MemoryRegionInfo::eNo);
  region.SetExecutable(MemoryRegionInfo::eNo);
  region.SetMapped(MemoryRegionInfo::eNo);
  return region;
}

MemoryRegionInfo
MinidumpParser::GetMemoryRegionInfo(lldb::addr_t load_addr) {
  if (!m_parsed_regions)
    GetMemoryRegions();
  return FindMemoryRegion(load_addr);
}

const MemoryRegionInfos &MinidumpParser::GetMemoryRegions() {
  if (!m_parsed_regions) {
    m_parsed_regions = true;
    // We haven't cached our memory regions yet we will create the region cache
    // once. We create the region cache using the best source. We start with
    // the linux maps since they are the most complete and have names for the
    // regions. Next we try the MemoryInfoList since it has
    // read/write/execute/map data, and then fall back to the MemoryList and
    // Memory64List to just get a list of the memory that is mapped in this
    // core file
    if (!CreateRegionsCacheFromLinuxMaps(*this, m_regions))
      if (!CreateRegionsCacheFromMemoryInfoList(*this, m_regions))
        if (!CreateRegionsCacheFromMemoryList(*this, m_regions))
          CreateRegionsCacheFromMemory64List(*this, m_regions);
    llvm::sort(m_regions.begin(), m_regions.end());
  }
  return m_regions;
}

#define ENUM_TO_CSTR(ST) case (uint32_t)StreamType::ST: return #ST

llvm::StringRef
MinidumpParser::GetStreamTypeAsString(uint32_t stream_type) {
  switch (stream_type) {
    ENUM_TO_CSTR(Unused);
    ENUM_TO_CSTR(ThreadList);
    ENUM_TO_CSTR(ModuleList);
    ENUM_TO_CSTR(MemoryList);
    ENUM_TO_CSTR(Exception);
    ENUM_TO_CSTR(SystemInfo);
    ENUM_TO_CSTR(ThreadExList);
    ENUM_TO_CSTR(Memory64List);
    ENUM_TO_CSTR(CommentA);
    ENUM_TO_CSTR(CommentW);
    ENUM_TO_CSTR(HandleData);
    ENUM_TO_CSTR(FunctionTable);
    ENUM_TO_CSTR(UnloadedModuleList);
    ENUM_TO_CSTR(MiscInfo);
    ENUM_TO_CSTR(MemoryInfoList);
    ENUM_TO_CSTR(ThreadInfoList);
    ENUM_TO_CSTR(HandleOperationList);
    ENUM_TO_CSTR(Token);
    ENUM_TO_CSTR(JavascriptData);
    ENUM_TO_CSTR(SystemMemoryInfo);
    ENUM_TO_CSTR(ProcessVMCounters);
    ENUM_TO_CSTR(BreakpadInfo);
    ENUM_TO_CSTR(AssertionInfo);
    ENUM_TO_CSTR(LinuxCPUInfo);
    ENUM_TO_CSTR(LinuxProcStatus);
    ENUM_TO_CSTR(LinuxLSBRelease);
    ENUM_TO_CSTR(LinuxCMDLine);
    ENUM_TO_CSTR(LinuxEnviron);
    ENUM_TO_CSTR(LinuxAuxv);
    ENUM_TO_CSTR(LinuxMaps);
    ENUM_TO_CSTR(LinuxDSODebug);
    ENUM_TO_CSTR(LinuxProcStat);
    ENUM_TO_CSTR(LinuxProcUptime);
    ENUM_TO_CSTR(LinuxProcFD);
    ENUM_TO_CSTR(FacebookAppCustomData);
    ENUM_TO_CSTR(FacebookBuildID);
    ENUM_TO_CSTR(FacebookAppVersionName);
    ENUM_TO_CSTR(FacebookJavaStack);
    ENUM_TO_CSTR(FacebookDalvikInfo);
    ENUM_TO_CSTR(FacebookUnwindSymbols);
    ENUM_TO_CSTR(FacebookDumpErrorLog);
    ENUM_TO_CSTR(FacebookAppStateLog);
    ENUM_TO_CSTR(FacebookAbortReason);
    ENUM_TO_CSTR(FacebookThreadName);
    ENUM_TO_CSTR(FacebookLogcat);
  }
  return "unknown stream type";
}
