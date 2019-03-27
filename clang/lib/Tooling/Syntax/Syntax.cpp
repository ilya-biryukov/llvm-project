//===- Syntax.cpp - mutable syntax trees ----------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "clang/Tooling/Syntax/Syntax.h"

using namespace clang;

void syntax::traverse(Node *N, llvm::function_ref<void(Node *)> Visit) {
  if (auto *T = llvm::dyn_cast<TreeNode>(N)) {
    for (auto *C : T->children())
      traverse(C, Visit);
  }
  Visit(N);
}
void syntax::traverse(const Node *N,
                      llvm::function_ref<void(const Node *)> Visit) {
  return traverse(const_cast<Node *>(N), [Visit](Node *N) { return Visit(N); });
}
