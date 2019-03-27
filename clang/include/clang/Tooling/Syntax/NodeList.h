//===- NodeList.h ---------------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLING_SYNTAX_NODELIST_H
#define LLVM_CLANG_TOOLING_SYNTAX_NODELIST_H

#include "llvm/Support/Allocator.h"
#include <algorithm>
#include <type_traits>

namespace clang {
namespace syntax {
namespace detail {
/// A vector that requests memory from BumpPtrAllocator and has a trivial
/// destructor. Syntax tree nodes use it to store children.
template <class T> class BumpAllocVector {
  // Make sure the elements are allowed to drop destructors.
  static_assert(std::is_trivially_destructible<T>::value,
                "T must be trivially destructible");
  // Implementation for trivially-copyable types is much simpler.
  static_assert(std::is_trivially_copyable<T>::value,
                "T must be trivially copyable.");

public:
  T *begin() { return Begin; }
  T *end() { return End; }

  const T *begin() const { return Begin; }
  const T *end() const { return End; }

  size_t size() const { return End - Begin; }
  bool empty() const { return begin() == end(); }

  void push_back(llvm::BumpPtrAllocator &A, T Element) {
    if (StorageEnd == End)
      Grow(A);
    *End = Element;
    ++End;
  }

  void erase(T *Begin, T *End) {
    std::ptrdiff_t ErasedSize = End - Begin;
    for (auto *It = End; It != this->End; ++It) {
      *Begin = It;
      ++Begin;
    }
    End -= ErasedSize;
  }

private:
  void Grow(llvm::BumpPtrAllocator &A) {
    size_t Size = End - Begin;

    size_t NewCapacity = 2 * (StorageEnd - Begin);
    if (NewCapacity == 0)
      NewCapacity = 1;

    T *NewStorage = A.Allocate<T>(NewCapacity);
    std::copy(Begin, End, NewStorage);

    A.Deallocate(Begin, StorageEnd - Begin);

    Begin = NewStorage;
    End = NewStorage + Size;
    StorageEnd = NewStorage + NewCapacity;
  }

private:
  T *Begin = nullptr;
  T *End = nullptr;
  T *StorageEnd = nullptr;
};
} // namespace detail

class Node;
/// Like vector<Node*>, but allocates all the memory in the BumpPtrAllocator.
/// Can be dropped without running the destructor.
using NodeList = detail::BumpAllocVector<Node *>;
} // namespace syntax
} // namespace clang

#endif
