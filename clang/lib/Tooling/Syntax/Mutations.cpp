//===- Mutations.cpp ------------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "clang/Tooling/Syntax/Mutations.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Token.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Syntax/BuildTree.h"
#include "clang/Tooling/Syntax/Nodes.h"
#include "clang/Tooling/Syntax/Tokens.h"
#include "clang/Tooling/Syntax/Tree.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Casting.h"
#include <cassert>
#include <string>

using namespace clang;

// This class has access to the internals of tree nodes. Its sole purpose is to
// define helpers that allow implementing the high-level mutation operations.
class syntax::MutationsImpl {
public:
  /// Add a new node with a specified role.
  static void addAfter(syntax::Node *Anchor, syntax::Node *New, NodeRole Role) {
    assert(Anchor != nullptr);
    assert(New->Parent == nullptr);
    assert(New->NextSibling == nullptr);
    assert(!New->isDetached());
    assert(Role != NodeRole::Detached);

    New->Role = static_cast<unsigned>(Role);
    auto *P = Anchor->parent();
    P->replaceChildRangeLowLevel(Anchor, Anchor, New);

    P->assertInvariants();
  }

  /// Replace the node, keeping the role.
  static void replace(syntax::Node *Old, syntax::Node *New) {
    assert(Old != nullptr);
    assert(Old->Parent != nullptr);
    assert(Old->canModify());
    assert(New->Parent == nullptr);
    assert(New->NextSibling == nullptr);
    assert(New->isDetached());

    New->Role = Old->Role;
    auto *P = Old->parent();
    P->replaceChildRangeLowLevel(findPrevious(Old), Old->nextSibling(), New);

    P->assertInvariants();
  }

  /// Completely remove the node from its parent.
  static void remove(syntax::Node *N) {
    auto *P = N->parent();
    P->replaceChildRangeLowLevel(findPrevious(N), N->nextSibling(),
                                 /*New=*/nullptr);

    P->assertInvariants();
    N->assertInvariants();
  }

private:
  static syntax::Node *findPrevious(syntax::Node *N) {
    if (N->parent()->firstChild() == N)
      return nullptr;
    for (syntax::Node *C = N->parent()->firstChild(); C != nullptr;
         C = C->nextSibling()) {
      if (C->nextSibling() == N)
        return C;
    }
    llvm_unreachable("could not find a child node");
  }
};

void syntax::removeStatement(syntax::Arena &A, syntax::Statement *S) {
  assert(S);
  assert(S->canModify());

  if (isa<CompoundStatement>(S->parent())) {
    // A child of CompoundStatement can just be safely removed.
    MutationsImpl::remove(S);
    return;
  }
  // For the rest, we have to replace with an empty statement.
  if (isa<EmptyStatement>(S))
    return; // already an empty statement, nothing to do.

  MutationsImpl::replace(S, createEmptyStatement(A));
}

namespace {
bool replaceIntroducesDanglingElse(syntax::Statement *Old,
                                   syntax::Statement *New) {
  auto *OldIf = dyn_cast<syntax::IfStatement>(Old->parent());
  // FIXME: what if we're an inner statement?
  if (!OldIf || Old->role() != syntax::NodeRole::IfStatement_thenStatement)
    return false;
  // FIXME: what if we're introducing this problem higher up the chain?
  if (!OldIf->elseKeyword())
    return false;
  // FIXME: what if the problem is lower in the tree?
  auto *NewIf = dyn_cast<syntax::IfStatement>(New);
  if (!NewIf)
    return false;
  // FIXME: what if the problem is lower in the else subtree?
  if (NewIf->elseKeyword())
    return false;
  return true;
}
} // namespace

syntax::Statement *syntax::replaceStatement(syntax::Arena &A,
                                            syntax::Statement *Old,
                                            syntax::Statement *New) {
  assert(Old);
  assert(New);
  assert(New->canModify());
  assert(New->isDetached());

  if (replaceIntroducesDanglingElse(Old, New)) {
    auto *Wrapper = createCompoundStatement(A);
    prependChildStatement(A, Wrapper, New);
    New = Wrapper;
  }

  MutationsImpl::replace(Old, New);
  return New;
}

void syntax::prependChildStatement(syntax::Arena &A,
                                   syntax::CompoundStatement *Parent,
                                   syntax::Statement *New) {
  assert(Parent);
  assert(New);
  assert(New->isDetached());

  MutationsImpl::addAfter(Parent->lbrace(), New,
                          NodeRole::CompoundStatement_statement);
}