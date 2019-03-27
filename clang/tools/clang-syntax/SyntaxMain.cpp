//===- SyntaxMain.cpp -----------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "Examples.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/Execution.h"
#include "clang/Tooling/Syntax/Syntax.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

namespace {
llvm::cl::opt<bool> DumpTree("dump-tree",
                             llvm::cl::desc("dump the syntax tree"),
                             llvm::cl::init(false));
llvm::cl::opt<bool> DumpTokens("dump-tokens",
                               llvm::cl::desc("dump the preprocessed tokens"),
                               llvm::cl::init(false));
llvm::cl::opt<bool> Transform(
    "transform",
    llvm::cl::desc(
        "apply sample transformations to the syntax tree and print the result"),
    llvm::cl::init(false));

class BuildSyntaxTree : public ASTFrontendAction {
public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    class Consumer : public ASTConsumer {
    public:
      Consumer(CompilerInstance &CI) : Tokens(CI.getPreprocessor()) {}

      void HandleTranslationUnit(ASTContext &AST) override {
        auto &SM = AST.getSourceManager();
        auto &LO = AST.getLangOpts();
        StringRef Filename =
            SM.getFileEntryForID(SM.getMainFileID())->getName();
        StringRef Code = SM.getBufferData(SM.getMainFileID());

        syntax::Corpus C(SM, LO, std::move(Tokens).consume());
        llvm::errs() << "--- Processing " << Filename << "\n";
        if (DumpTokens) {
          llvm::errs() << "== Tokens after preprocessing:\n";
          for (auto T : C.mainFile().tokens())
            llvm::errs() << T.text(SM) << " ";
          llvm::errs() << "\n";
          llvm::errs() << "== Macro expansions:\n";
          for (auto E : C.mainFile().expansions()) {
            for (auto T : E.macroTokens(C.mainFile()))
              llvm::errs() << T.text(SM) << " ";
            llvm::errs() << " -> ";
            for (auto T : E.tokens(C.mainFile()))
              llvm::errs() << T.text(SM) << " ";
            llvm::errs() << "\n";
          }
        }
        auto *TU = syntax::buildSyntaxTree(C, *AST.getTranslationUnitDecl());
        if (DumpTree) {
          llvm::errs() << "== Syntax tree:\n";
          TU->dump(C, llvm::errs());
        }
        if (!Transform)
          return;

        applyTransforms(C, TU);
        if (DumpTokens) {
          llvm::errs() << "== Tokens after applying transformations:\n";
          TU->dumpTokens(C, llvm::errs());
        }
        if (DumpTree) {
          llvm::errs() << "== Tree after applying transformations:\n";
          TU->dump(C, llvm::errs());
        }

        auto R = computeReplacements(C, TU);
        llvm::outs() << llvm::cantFail(tooling::applyAllReplacements(Code, R))
                     << "\n";
      }

    private:
      syntax::TokenCollector Tokens;
    };
    return llvm::make_unique<Consumer>(CI);
  }
};

class Factory : public tooling::FrontendActionFactory {
  FrontendAction *create() override { return new BuildSyntaxTree; }
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
  if (!Transform && !DumpTokens && !DumpTree) {
    llvm::errs() << "Please specify at least one of -transform, -dump-tree or "
                    "-dump-tokens\n";
    return 1;
  }
  // Collect symbols found in each translation unit, merging as we go.
  auto Err = Executor->get()->execute(llvm::make_unique<Factory>());
  if (Err)
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  return 0;
}
