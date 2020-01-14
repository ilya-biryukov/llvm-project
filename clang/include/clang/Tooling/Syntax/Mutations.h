//===- Mutations.h - mutate syntax trees --------------------*- C++ ---*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// Defines high-level APIs for transforming syntax trees and producing the
// corresponding textual replacements.
//===----------------------------------------------------------------------===//
#ifndef LLVM_CLANG_TOOLING_SYNTAX_MUTATIONS_H
#define LLVM_CLANG_TOOLING_SYNTAX_MUTATIONS_H

#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Syntax/Nodes.h"
#include "clang/Tooling/Syntax/Tree.h"

namespace clang {
namespace syntax {

/// Computes textual replacements required to mimic the tree modifications made
/// to the syntax tree.
tooling::Replacements computeReplacements(const Arena &A,
                                          const syntax::TranslationUnit &TU);

/// Removes a statement or replaces it with an empty statement where one is
/// required syntactically. E.g., in the following example:
///     if (cond) { foo(); } else bar();
/// One can remove `foo();` completely and to remove `bar();` we would need to
/// replace it with an empty statement.
/// EXPECTS: S->canModify() == true
void removeStatement(syntax::Arena &A, syntax::Statement *S);

/// Replaces a statement \p Old with \p New.
/// FIXME: this does not handle corner cases that might change semantics, e.g.
///        introduce dangling else when replacing compound statement with an
///        if statement. Add braces in those cases.
/// Returns the new statement that replaced \p Old. It is either \p New or a
/// compound statement, wrapping \p New to avoid danlging else.
///
/// EXPECTS: Old->canModify() && New->isDetached() && New->canModify()
syntax::Statement *replaceStatement(syntax::Arena &A, syntax::Statement *Old,
                                    syntax::Statement *New);

/// FIXME: add canPrepend() check.
/// EXPECTS: New->isDetached()
void prependChildStatement(syntax::Arena &A, syntax::CompoundStatement *Parent,
                           syntax::Statement *New);

} // namespace syntax
} // namespace clang

#endif
