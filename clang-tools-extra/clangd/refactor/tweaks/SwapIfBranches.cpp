//===--- SwapIfBranches.cpp --------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "Logger.h"
#include "ParsedAST.h"
#include "SourceCode.h"
#include "refactor/Tweak.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Syntax/Mutations.h"
#include "clang/Tooling/Syntax/Nodes.h"
#include "clang/Tooling/Syntax/Tree.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"

namespace clang {
namespace clangd {
namespace {
/// Swaps the 'then' and 'else' branch of the if statement.
/// Before:
///   if (foo) { return 10; } else { continue; }
///   ^^^^^^^                 ^^^^
/// After:
///   if (foo) { continue; } else { return 10; }
class SwapIfBranches : public Tweak {
public:
  const char *id() const override final;

  bool prepare(const Selection &Inputs) override;
  Expected<Effect> apply(const Selection &Inputs) override;
  std::string title() const override { return "Swap if branches"; }
  Intent intent() const override { return Refactor; }

private:
  syntax::IfStatement *If = nullptr;
};

REGISTER_TWEAK(SwapIfBranches)

bool SwapIfBranches::prepare(const Selection &Inputs) {
  for (auto *N = Inputs.SelectedNode; N && !If; N = N->parent()) {
    // Stop once we hit a block, e.g. a lambda in the if condition.
    if (isa<syntax::CompoundStatement>(N))
      return false;
    If = dyn_cast<syntax::IfStatement>(N);
  }
  if (!If)
    return false;
  auto *Then = If->thenStatement();
  if (!Then || !Then->canModify())
    return false;
  auto *Else = If->elseStatement();
  if (!Else || !Else->canModify())
    return false;
  return true;
}

Expected<Tweak::Effect> SwapIfBranches::apply(const Selection &Inputs) {
  auto &A = const_cast<syntax::Arena&>(Inputs.SynArena);

  auto *Then = If->thenStatement();
  auto *Else = If->elseStatement();

  syntax::removeStatement(A, Then);
  syntax::removeStatement(A, Else);

  syntax::replaceStatement(A, If->thenStatement(), Else);
  syntax::replaceStatement(A, If->elseStatement(), Then);

  return Effect::mainFileEdit(A.sourceManager(),
                              syntax::computeReplacements(A, *Inputs.SynTU));
}

} // namespace
} // namespace clangd
} // namespace clang
