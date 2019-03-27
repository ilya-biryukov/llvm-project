//===- ComputeReplacements.cpp --------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Tooling/Syntax/Syntax.h"

using namespace clang;

namespace {
/// Merges consecutive inserts and removals into non-conflicting
/// tooling::Replacement instances.
class BuildReplacements {
public:
  BuildReplacements(StringRef File) : File(File) {}
  tooling::Replacements consume() && {
    if (InProgress)
      llvm::cantFail(Changes.add(*InProgress)); // Flush the oustanding edit.
    return std::move(Changes);
  }

  void insert(unsigned Offset, StringRef Text) {
    flushIfRequired(Offset);
    if (InProgress) {
      InProgress = tooling::Replacement(
          File, InProgress->getOffset(), InProgress->getLength(),
          (InProgress->getReplacementText() + Text).str());
    } else {
      InProgress = tooling::Replacement(File, Offset, /*Length=*/0, Text);
    }
  };

  void remove(unsigned BeginOffset, unsigned EndOffset) {
    flushIfRequired(BeginOffset);
    unsigned Length = EndOffset - BeginOffset;
    if (InProgress) {
      InProgress = tooling::Replacement(File, InProgress->getOffset(),
                                        InProgress->getLength() + Length,
                                        InProgress->getReplacementText());
    } else {
      InProgress = tooling::Replacement(File, BeginOffset, Length,
                                        /*ReplacementText=*/"");
    }
  };

private:
  /// Insert() and Remove() will merge consecutive insertions and removals
  /// into non-conflicting calls to tooling::Replacements.
  void flushIfRequired(unsigned NextChangeOffset) {
    if (!InProgress ||
        InProgress->getOffset() + InProgress->getLength() == NextChangeOffset) {
      return;
    }
    assert(InProgress->getOffset() + InProgress->getLength() <
           NextChangeOffset);
    // No conflict anymore, push the replacement into the final changes.
    llvm::cantFail(Changes.add(*InProgress));
    InProgress.reset();
  };

  tooling::Replacements Changes;
  StringRef File;
  llvm::Optional<tooling::Replacement> InProgress;
};
} // namespace

tooling::Replacements syntax::computeReplacements(Corpus &C,
                                                  TranslationUnit *R) {
  auto &SM = C.sourceManager();
  auto &MainFile = C.mainFile();

  llvm::ArrayRef<Token> Tokens = MainFile.tokens();
  auto NextTok = Tokens.begin();

  BuildReplacements Changes(
      SM.getFileEntryForID(SM.getMainFileID())->getName());
  auto NextOffset = [&]() {
    auto *MC = MainFile.findMacroCall(NextTok);
    if (MC)
      return SM.getFileOffset(MC->macroTokens(MainFile).front().location());
    assert(NextTok->location().isFileID());
    return SM.getFileOffset(NextTok->location());
  };

  // Start and end of the consecutive tokens currently being inserted.
  const syntax::Token *BeginInserted = nullptr;
  const syntax::Token *EndInserted = nullptr;
  auto FlushInsertion = [&]() {
    if (!BeginInserted)
      return;
    // The next token is not consequent, emit an insertion at this point.
    Changes.insert(NextOffset(),
                   C.getText(llvm::makeArrayRef(BeginInserted, EndInserted)));

    BeginInserted = nullptr;
    EndInserted = nullptr;
  };
  // Traverse the tokens, skipping the unchanged ranges of the file and
  // recording all removals and insertions made to the token nodes.
  traverse(R, [&](Node *N) {
    auto *L = llvm::dyn_cast<Leaf>(N);
    if (!L)
      return;
    if (L->isOriginal()) {
      FlushInsertion();
      assert(NextTok <= L->tokens().begin());
      if (NextTok != L->tokens().begin()) {
        /// We found a gap, record a removal.
        auto Range = MainFile.toOffsetRange(NextTok, L->tokens().data(), SM);
        assert(Range);
        Changes.remove(Range->first, Range->second);
      }
      NextTok = L->tokens().end();
      return;
    }
    if (BeginInserted) {
      if (EndInserted == L->tokens().data()) {
        // This is a continuation of the tracked range, so just extend it.
        EndInserted = L->tokens().data() + L->tokens().size();
        return;
      }
      FlushInsertion();
    }
    assert(!BeginInserted);
    assert(!EndInserted);
    BeginInserted = L->tokens().data();
    EndInserted = BeginInserted + L->tokens().size();
  });
  FlushInsertion();
  // Handle removals at the end of file.
  if (NextTok != Tokens.end())
    Changes.remove(NextOffset(), SM.getFileOffset(Tokens.back().endLocation()));
  return std::move(Changes).consume();
}
