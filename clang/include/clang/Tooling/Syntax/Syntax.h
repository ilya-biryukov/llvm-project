//===- Syntax.h - mutable syntax trees ------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_CLANG_TOOLING_SYNTAX_SYNTAX_H
#define LLVM_CLANG_TOOLING_SYNTAX_SYNTAX_H

#include "clang/AST/Decl.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Syntax/Corpus.h"
#include "clang/Tooling/Syntax/Nodes.h"

namespace clang {
namespace syntax {

/// Perform a post-order traversal of the syntax tree, calling \p Visit at each
/// node.
void traverse(Node *N, llvm::function_ref<void(Node *)> Visit);
void traverse(const Node *N, llvm::function_ref<void(const Node *)> Visit);

/// Build a syntax tree for the main file.
TranslationUnit *buildSyntaxTree(Corpus &C,
                                 const clang::TranslationUnitDecl &TU);

/// Compute the replacements required that would transform the original document
/// (i.e. the main file) to match the modifications done to the tree.
tooling::Replacements computeReplacements(Corpus &C, TranslationUnit *R);

} // namespace syntax
} // namespace clang
#endif
