//===- Examples.h ---------------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_CLANGSYNTAX_TRANSFORMATIONS_H
#define LLVM_CLANG_TOOLS_CLANGSYNTAX_TRANSFORMATIONS_H

#include "clang/Tooling/Syntax/Syntax.h"

namespace clang {
// Recursively apply example transformations on \p Node.
// The purpose of moving this out of SyntaxMain.cpp is to make it easy to
// isolate the syntax tree mutations code from non-interesting details.
void applyTransforms(syntax::Corpus &C, syntax::TreeNode *Node);
} // namespace clang

#endif
