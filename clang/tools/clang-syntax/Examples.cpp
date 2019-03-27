//===- Examples.cpp -------------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Examples.h"
#include "clang/Basic/TokenKinds.def"
#include "clang/Tooling/Syntax/Syntax.h"
#include "clang/Tooling/Syntax/Transform.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"

using namespace clang;

namespace {
void swapBinaryArguments(syntax::Corpus &C, syntax::BinaryExpr *E) {
  if (!E->lhs()->isCopyable(C) || !E->rhs()->isCopyable(C))
    return;

  auto *L = cast<syntax::Expr>(E->lhs()->copy(C));
  auto *R = cast<syntax::Expr>(E->rhs()->copy(C));

  (void)syntax::replaceExpression(C, E->lhs(), R);
  (void)syntax::replaceExpression(C, E->rhs(), L);
}

/// Replaces a*b with a+b and vice versa. Can parenthesize if necessary.
LLVM_NODISCARD syntax::Expr *swapMultipliesAndAdditions(syntax::Corpus &C,
                                                        syntax::BinaryExpr *E) {
  auto Operator = E->operatorKind();
  if (Operator != tok::plus && Operator != tok::star)
    return E;
  if (!E->lhs()->isCopyable(C) || !E->rhs()->isCopyable(C))
    return E;
  auto *New = syntax::createBinaryExpr(
      C, E->lhs(), Operator == tok::plus ? tok::star : tok::plus, E->rhs());
  return syntax::replaceExpression(C, E, New);
}

void removeParentheses(syntax::Corpus &C, syntax::ParenExpr *E) {
  if (!E->inner()->isCopyable(C) || !E->isModifiable(C))
    return;
  E->parent()->replaceChild(C, E, E->inner());
}
} // namespace

void clang::applyTransforms(syntax::Corpus &C, syntax::TreeNode *Node) {
  for (auto *Child : Node->children()) {
    auto *T = llvm::dyn_cast<syntax::TreeNode>(Child);
    if (!T)
      continue;
    applyTransforms(C, T);
  }
  (void)removeParentheses;
  // if (auto *E = llvm::dyn_cast<syntax::ParenExpr>(Node))
  //   removeParentheses(E);
  if (auto *Binary = llvm::dyn_cast<syntax::BinaryExpr>(Node)) {
    Node = swapMultipliesAndAdditions(C, Binary);
    (void)swapBinaryArguments;
    // swapBinaryArguments(C, Binary);
  }
}
