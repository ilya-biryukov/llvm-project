//===- Synthesis.cpp ------------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "clang/Basic/TokenKinds.h"
#include "clang/Tooling/Syntax/BuildTree.h"
#include "clang/Tooling/Syntax/Nodes.h"
#include "clang/Tooling/Syntax/Tree.h"

using namespace clang;

/// Exposes private syntax tree APIs required to implement node synthesis.
/// Should not be used for anything else.
class syntax::FactoryImpl {
public:
  static void setCanModify(syntax::Node *N) { N->CanModify = true; }

  static void prependChildLowLevel(syntax::Tree *T, syntax::Node *Child,
                                   syntax::NodeRole R) {
    T->prependChildLowLevel(Child, R);
  }
};

clang::syntax::Leaf *syntax::createPunctuation(clang::syntax::Arena &A,
                                               clang::tok::TokenKind K) {
  auto Tokens = A.lexBuffer(llvm::MemoryBuffer::getMemBuffer(
                                clang::tok::getPunctuatorSpelling(K)))
                    .second;
  assert(Tokens.size() == 1);
  assert(Tokens.front().kind() == K);
  auto *L = new (A.allocator()) clang::syntax::Leaf(Tokens.begin());
  FactoryImpl::setCanModify(L);
  L->assertInvariants();
  return L;
}

clang::syntax::EmptyStatement *
syntax::createEmptyStatement(clang::syntax::Arena &A) {
  auto *S = new (A.allocator()) clang::syntax::EmptyStatement;
  FactoryImpl::setCanModify(S);
  FactoryImpl::prependChildLowLevel(S, createPunctuation(A, clang::tok::semi),
                                    NodeRole::Unknown);
  S->assertInvariants();
  return S;
}

syntax::CompoundStatement *createCompoundStatement(clang::syntax::Arena &A) {
  auto *S = new (A.allocator()) clang::syntax::CompoundStatement;
  syntax::FactoryImpl::setCanModify(S);
  syntax::FactoryImpl::prependChildLowLevel(
      S, syntax::createPunctuation(A, clang::tok::r_brace),
      syntax::NodeRole::OpenParen);
  syntax::FactoryImpl::prependChildLowLevel(
      S, syntax::createPunctuation(A, clang::tok::l_brace),
      syntax::NodeRole::CloseParen);
  S->assertInvariants();
  return S;
}