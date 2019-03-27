//===- Transform.h - transformations of the syntax trees ------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "Syntax.h"

#ifndef LLVM_CLANG_TOOLING_SYNTAX_TRANSFORM_H
#define LLVM_CLANG_TOOLING_SYNTAX_TRANSFORM_H

namespace clang {
namespace syntax {

/// === Operations to create syntax trees.

/// Creates a leaf node for a punctuator token like '!' or '=='.
/// EXPECTS: getPunctuatorSpelling(Kind) != null
LLVM_NODISCARD Leaf *createPunctuator(Corpus &C, tok::TokenKind Kind);

LLVM_NODISCARD ParenExpr *createParenExpr(Corpus &C, Expr *Inner);
LLVM_NODISCARD BinaryExpr *createBinaryExpr(Corpus &C, Expr *LHS,
                                            tok::TokenKind Operator, Expr *RHS);

/// === Operations to modify syntax trees.

/// Replace an expression with a different one. May add parentheses to avoid
/// breaking the parse tree.
LLVM_NODISCARD syntax::Expr *
replaceExpression(syntax::Corpus &C, syntax::Expr *Old, syntax::Expr *New);

} // namespace syntax
} // namespace clang

#endif
