//===- SyntaxMain.cpp -----------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/Execution.h"
#include "clang/Tooling/Syntax/BuildTree.h"
#include "clang/Tooling/Syntax/Tokens.h"
#include "clang/Tooling/Syntax/Tree.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

using namespace clang;

namespace {
llvm::cl::opt<bool> DumpTokens("dump-tokens",
                               llvm::cl::desc("dump the preprocessed tokens"),
                               llvm::cl::init(false));
llvm::cl::opt<bool> DumpTree("dump-tree",
                             llvm::cl::desc("dump the syntax tree"),
                             llvm::cl::init(false));

class BuildSyntaxTree : public ASTFrontendAction {
public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    class Consumer : public ASTConsumer {
    public:
      Consumer(CompilerInstance &CI) : Collector(CI.getPreprocessor()) {}

      void HandleTranslationUnit(ASTContext &AST) override {
        syntax::Arena A(AST.getSourceManager(), AST.getLangOpts(),
                        std::move(Collector).consume());
        auto *TU = syntax::buildSyntaxTree(A, *AST.getTranslationUnitDecl());
        if (DumpTokens)
          llvm::outs() << A.tokenBuffer().dumpForTests();
        if (DumpTree)
          llvm::outs() << TU->dump(A);
      }

    private:
      syntax::TokenCollector Collector;
    };
    return std::make_unique<Consumer>(CI);
  }
};

class Factory : public tooling::FrontendActionFactory {
  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<BuildSyntaxTree>();
  }
};

} // namespace

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);
  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
      argc, argv, llvm::cl::GeneralCategory,
      "Build syntax trees for the specified files");
  if (!Executor) {
    llvm::errs() << llvm::toString(Executor.takeError()) << "\n";
    return 1;
  }
  if (!DumpTokens && !DumpTree) {
    llvm::errs()
        << "Please specify at least one of -dump-tree or -dump-tokens\n";
    return 1;
  }
  // Collect symbols found in each translation unit, merging as we go.
  auto Err = Executor->get()->execute(std::make_unique<Factory>());
  if (Err)
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  return 0;
}
