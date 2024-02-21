//===---- tools/extra/ToolTemplate.cpp - Template for refactoring tool ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements an empty refactoring tool using the clang tooling.
//  The goal is to lower the "barrier to entry" for writing refactoring tools.
//
//  Usage:
//  tool-template <cmake-output-dir> <file1> <file2> ...
//
//  Where <cmake-output-dir> is a CMake build directory in which a file named
//  compile_commands.json exists (enable -DCMAKE_EXPORT_COMPILE_COMMANDS in
//  CMake to get this output).
//
//  <file1> ... specify the paths of files in the CMake source tree. This path
//  is looked up in the compile command database. If the path of a file is
//  absolute, it needs to point into CMake's source tree. If the path is
//  relative, the current working directory needs to be in the CMake source
//  tree and the file must be in a subdirectory of the current working
//  directory. "./" prefixes in the relative files will be automatically
//  removed, but the rest of a relative path must be a suffix of a path in
//  the compile command line database.
//
//  For example, to use tool-template on all files in a subtree of the
//  source tree, use:
//
//    /path/in/subtree $ find . -name '*.cpp'|
//        xargs tool-template /path/to/build
//
//===----------------------------------------------------------------------===//

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Execution.h"
#include "clang/Tooling/Refactoring/AtomicChange.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

namespace {
class ToolTemplateCallback : public MatchFinder::MatchCallback {
public:
  ToolTemplateCallback(ExecutionContext &Context) : Context(Context) {}

  void run(const MatchFinder::MatchResult &Result) override {
    auto *D = Result.Nodes.getNodeAs<NamedDecl>("member");
    assert(D);

    std::string DeclDump;
    {
      llvm::raw_string_ostream OS(DeclDump);
      OS << D->getDeclName();
    }
    Context.reportResult(getAccessSpelling(D->getAccess()), DeclDump);
  }
private:
  ExecutionContext &Context;
};
} // end anonymous namespace

// Set up the command line options
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::OptionCategory ToolTemplateCategory("dump-by-access options");

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
      argc, argv, ToolTemplateCategory);

  if (!Executor) {
    llvm::errs() << llvm::toString(Executor.takeError()) << "\n";
    return 1;
  }

  ast_matchers::MatchFinder Finder;
  ToolTemplateCallback Callback(*Executor->get()->getExecutionContext());

  // TODO: Put your matchers here.
  // Use Finder.addMatcher(...) to define the patterns in the AST that you
  // want to match against. You are not limited to just one matcher!
  //
  // This is a sample matcher:
  Finder.addMatcher(
      namedDecl(hasParent(namedDecl(hasName("clang::Sema")))).bind("member"),
      &Callback);

  auto Err = Executor->get()->execute(newFrontendActionFactory(&Finder));
  if (Err) {
    llvm::errs() << "error:" << llvm::toString(std::move(Err)) << "\n";
    return 1;
  }

  constexpr int LastAccessSpec = static_cast<int>(AS_none);
  std::vector<std::string> Decls[LastAccessSpec + 1];
  Executor->get()->getToolResults()->forEachResult(
      [&](llvm::StringRef K, llvm::StringRef V) {
        for (int A = 0; A <= LastAccessSpec; ++A) {
          if (K == getAccessSpelling(static_cast<AccessSpecifier>(A))) {
            Decls[A].push_back(V.str());
            return;
          }
        }
        llvm::errs() << "error: unhandled access specifier " << K << "\n";
      });

  llvm::outs() << "Member declarations of Sema by access level:\n";
  for (int A = 0; A <= LastAccessSpec; ++A) {
    if (Decls[A].empty()) continue;

    std::sort(Decls[A].begin(), Decls[A].end());
    llvm::outs() << getAccessSpelling(static_cast<AccessSpecifier>(A)) << " (" << Decls[A].size() << " declarations) {\n";
    for (StringRef D : Decls[A])
      llvm::outs() << "  - " << D << "\n";
    llvm::outs()  << "}\n";
  }
  return 0;
}
