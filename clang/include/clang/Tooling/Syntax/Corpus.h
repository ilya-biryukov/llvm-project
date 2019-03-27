//===- Corpus.h - memory arena and bookkeeping for syntax trees --- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_CLANG_TOOLING_SYNTAX_CORPUS_H
#define LLVM_CLANG_TOOLING_SYNTAX_CORPUS_H

#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Tooling/Syntax/TokenBuffer.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Allocator.h"

namespace clang {
namespace syntax {
/// A corpus is a memory arena for a set of syntax trees. In addition, it also
/// tracks the underlying token buffers, manager, etc.
class Corpus {
public:
  Corpus(SourceManager &SourceMgr, const LangOptions &LangOpts,
         TokenBuffer MainFile);

  const SourceManager &sourceManager() const { return SourceMgr; }
  const LangOptions &langOptions() const { return LangOpts; }

  /// Construct a new syntax node of a specified kind. The memory for a node is
  /// owned by the corpus and will be freed when the corpus is destroyed.
  template <class TNode, class... Args> TNode *construct(Args &&... As) {
    static_assert(std::is_trivially_destructible<TNode>::value,
                  "nodes should be trivially destructible");
    return new (Allocator) TNode(std::forward<Args>(As)...);
  }

  const TokenBuffer *findBuffer(FileID FID) const;
  const TokenBuffer *findBuffer(SourceLocation Loc) const;

  const TokenBuffer &mainFile() const;

  llvm::BumpPtrAllocator &allocator() { return Allocator; }

  /// Get a text for a continuous range of tokens.
  /// EXPECTS: Tokens are a part of some tokenized buffer from \p Buffers.
  llvm::StringRef getText(llvm::ArrayRef<syntax::Token> Tokens);

  /// Tokenize the passed buffer. This function runs a lexer in raw mode, i.e.
  /// the result won't contain any macro expansions, etc.
  std::pair<FileID, const TokenBuffer &>
  tokenizeBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer);

private:
  SourceManager &SourceMgr;
  const LangOptions &LangOpts;
  llvm::DenseMap<FileID, TokenBuffer> Buffers;
  /// Keeps all the allocated nodes.
  llvm::BumpPtrAllocator Allocator;
};

} // namespace syntax
} // namespace clang

#endif
