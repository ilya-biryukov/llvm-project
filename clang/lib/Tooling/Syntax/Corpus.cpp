//===- Corpus.cpp ---------------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "clang/Tooling/Syntax/Corpus.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Lex/Lexer.h"

using namespace clang;

syntax::Corpus::Corpus(SourceManager &SourceMgr, const LangOptions &LangOpts,
                       TokenBuffer MainFile)
    : SourceMgr(SourceMgr), LangOpts(LangOpts) {
  Buffers.try_emplace(SourceMgr.getMainFileID(), std::move(MainFile));
}

const syntax::TokenBuffer *syntax::Corpus::findBuffer(FileID FID) const {
  auto It = Buffers.find(FID);
  if (It == Buffers.end())
    return nullptr;
  return &It->second;
}

const syntax::TokenBuffer *
syntax::Corpus::findBuffer(SourceLocation Loc) const {
  return findBuffer(SourceMgr.getFileID(SourceMgr.getExpansionLoc(Loc)));
}

const clang::syntax::TokenBuffer &syntax::Corpus::mainFile() const {
  const auto *Buffer = findBuffer(SourceMgr.getMainFileID());
  assert(Buffer);
  return *Buffer;
}

llvm::StringRef syntax::Corpus::getText(llvm::ArrayRef<syntax::Token> Tokens) {
  if (Tokens.empty())
    return "";
  auto FID =
      SourceMgr.getFileID(SourceMgr.getExpansionLoc(Tokens.front().location()));
  auto Range =
      findBuffer(FID)->toOffsetRange(Tokens.begin(), Tokens.end(), SourceMgr);
  assert(Range && "token range crosses macro expansions");
  return SourceMgr.getBufferData(FID).substr(Range->first,
                                             Range->second - Range->first);
}

std::pair<FileID, const syntax::TokenBuffer &>
syntax::Corpus::tokenizeBuffer(std::unique_ptr<llvm::MemoryBuffer> Input) {
  auto FID = SourceMgr.createFileID(std::move(Input));

  // FIXME: we want to know the actual language options.
  LangOptions LO;
  auto It = Buffers.try_emplace(FID, tokenize(FID, SourceMgr, LO));
  assert(It.second && "duplicate FileID");
  return {FID, It.first->second};
}
