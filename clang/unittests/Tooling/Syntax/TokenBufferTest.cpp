//===- TokenBufferTest.cpp ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Tooling/Syntax/TokenBuffer.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Expr.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticIDs.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/FileSystemOptions.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TokenKinds.def"
#include "clang/Basic/TokenKinds.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/Token.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Testing/Support/Annotations.h"
#include <cassert>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <memory>
#include <ostream>
#include <string>

using namespace clang;
using namespace clang::syntax;

using ::testing::AllOf;
using ::testing::Contains;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Matcher;
using ::testing::Pointwise;

// Debug printers.
// FIXME: This should live somewhere else or be implemented as 'operator
// <<(raw_ostream&, T)'.
namespace clang {
namespace tok {
inline void PrintTo(TokenKind K, std::ostream *OS) {
  *OS << tok::getTokenName(K);
}
} // namespace tok
namespace syntax {
inline void PrintTo(const syntax::Token &T, std::ostream *OS) {
  PrintTo(T.kind(), OS);
  OS->flush();
}
} // namespace syntax
} // namespace clang

namespace {
// Matchers for clang::Token.
MATCHER_P(Kind, K, "") { return arg.kind() == K; }
MATCHER_P2(HasText, Text, SourceMgr, "") {
  return arg.text(*SourceMgr) == Text;
}
MATCHER_P2(IsIdent, Text, SourceMgr, "") {
  return arg.kind() == tok::identifier && arg.text(*SourceMgr) == Text;
}
/// Checks the start and end location of a token are equal to SourceRng.
MATCHER_P(RangeIs, SourceRng, "") {
  return arg.location() == SourceRng.first &&
         arg.endLocation() == SourceRng.second;
}
/// Checks the passed tuple has two similar tokens, i.e. both are of the same
/// kind and have the same text if they are identifiers.
MATCHER_P(IsSameToken, SourceMgr, "") {
  auto &L = std::get<0>(arg);
  auto &R = std::get<1>(arg);
  if (L.kind() != R.kind())
    return false;
  return L.text(*SourceMgr) == L.text(*SourceMgr);
}

class TokenBufferTest : public ::testing::Test {
public:
  /// Run the clang frontend, collect the preprocessed tokens from the frontend
  /// invocation and store them in this->Tokens.
  /// This also clears SourceManager before running the compiler.
  void recordTokens(llvm::StringRef Code) {
    class RecordTokens : public ASTFrontendAction {
    public:
      explicit RecordTokens(TokenBuffer &Result) : Result(Result) {}

      bool BeginSourceFileAction(CompilerInstance &CI) override {
        assert(!Collector && "expected only a single call to BeginSourceFile");
        Collector.emplace(CI.getPreprocessor());
        return true;
      }
      void EndSourceFileAction() override {
        assert(Collector && "BeginSourceFileAction was never called");
        Result = std::move(*Collector).consume();
      }

      std::unique_ptr<ASTConsumer>
      CreateASTConsumer(CompilerInstance &CI, StringRef InFile) override {
        return llvm::make_unique<ASTConsumer>();
      }

    private:
      TokenBuffer &Result;
      llvm::Optional<TokenCollector> Collector;
    };

    constexpr const char *FileName = "./input.cpp";
    FS->addFile(FileName, time_t(), llvm::MemoryBuffer::getMemBufferCopy(""));
    // Prepare to run a compiler.
    std::vector<const char *> Args = {"tok-test", "-std=c++03", "-fsyntax-only",
                                      FileName};
    auto CI = createInvocationFromCommandLine(Args, Diags, FS);
    assert(CI);
    CI->getFrontendOpts().DisableFree = false;
    CI->getPreprocessorOpts().addRemappedFile(
        FileName, llvm::MemoryBuffer::getMemBufferCopy(Code).release());
    LangOpts = *CI->getLangOpts();
    CompilerInstance Compiler;
    Compiler.setInvocation(std::move(CI));
    if (!Diags->getClient())
      Diags->setClient(new IgnoringDiagConsumer);
    Compiler.setDiagnostics(Diags.get());
    Compiler.setFileManager(FileMgr.get());
    Compiler.setSourceManager(SourceMgr.get());

    this->Buffer = TokenBuffer();
    RecordTokens Recorder(this->Buffer);
    ASSERT_TRUE(Compiler.ExecuteAction(Recorder))
        << "failed to run the frontend";
  }

  /// Run syntax::tokenize() and return the results.
  TokenBuffer tokenize(llvm::StringRef Text) {
    // Null-terminate so that we always see 'tok::eof' at the end.
    std::string NullTerminated = Text.str();
    auto FID = SourceMgr->createFileID(llvm::MemoryBuffer::getMemBufferCopy(
        StringRef(NullTerminated.data(), NullTerminated.size() + 1)));
    return syntax::tokenize(FID, *SourceMgr, LangOpts);
  }

  /// Checks that lexing \p ExpectedText in raw mode would produce the same
  /// token stream as the one stored in this->Buffer.tokens().
  void checkTokens(llvm::StringRef ExpectedText) {
    auto TokenizedCode = tokenize(ExpectedText);
    std::vector<syntax::Token> ExpectedTokens = TokenizedCode.tokens();
    EXPECT_THAT(std::vector<syntax::Token>(Buffer.tokens()),
                Pointwise(IsSameToken(), ExpectedTokens))
        << "\texpected tokens: " << ExpectedText;
  }

  struct ExpectedExpansion {
    ExpectedExpansion(std::string From, std::string To,
                      llvm::Optional<llvm::Range> Range = llvm::None)
        : From(std::move(From)), To(std::move(To)), Range(Range) {}
    /// A textual representation of the macro tokens.
    std::string From;
    /// A textual representation of the expansion result.
    std::string To;
    /// A text range the expansion points to.
    llvm::Optional<llvm::Range> Range;
  };
  /// Checks the expansions in this->Buffer.macroExpansions() match the \p
  /// Expected ones.
  void checkExpansions(llvm::ArrayRef<ExpectedExpansion> Expected) {
    auto Actual = Buffer.expansions();
    ASSERT_EQ(Actual.size(), Expected.size());

    for (unsigned I = 0; I < Actual.size(); ++I) {
      auto &A = Actual[I];
      auto &E = Expected[I];

      if (E.Range)
        ASSERT_EQ(A.macroRange(Buffer, *SourceMgr),
                  (std::pair<unsigned, unsigned>(E.Range->Begin, E.Range->End)))
            << "\trange does not match";

      ASSERT_THAT(
          std::vector<syntax::Token>(A.macroTokens(Buffer)),
          Pointwise(IsSameToken(), std::vector<syntax::Token>(
                                       tokenize(E.From).tokens().drop_back())))
          << "\tmacro tokens do not match, expected " << E.From;

      ASSERT_THAT(
          std::vector<syntax::Token>(A.tokens(Buffer)),
          Pointwise(IsSameToken(), std::vector<syntax::Token>(
                                       tokenize(E.To).tokens().drop_back())))
          << "\ttokens after expansion do not match, expected " << E.To;
    }
  }

  // Specialized versions of matchers that rely on SourceManager.
  Matcher<syntax::Token> IsIdent(std::string Text) const {
    return ::IsIdent(Text, SourceMgr.get());
  }
  Matcher<syntax::Token> HasText(std::string Text) const {
    return ::HasText(Text, SourceMgr.get());
  }
  Matcher<syntax::Token> RangeIs(llvm::Range R) const {
    std::pair<SourceLocation, SourceLocation> Ls;
    Ls.first = SourceMgr->getLocForStartOfFile(SourceMgr->getMainFileID())
                   .getLocWithOffset(R.Begin);
    Ls.second = SourceMgr->getLocForStartOfFile(SourceMgr->getMainFileID())
                    .getLocWithOffset(R.End);
    return ::RangeIs(Ls);
  }
  Matcher<std::tuple<const syntax::Token &, const syntax::Token &>>
  IsSameToken() const {
    return ::IsSameToken(SourceMgr.get());
  }

  // Data fields.
  llvm::IntrusiveRefCntPtr<DiagnosticsEngine> Diags =
      new DiagnosticsEngine(new DiagnosticIDs, new DiagnosticOptions);
  IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> FS =
      new llvm::vfs::InMemoryFileSystem;
  llvm::IntrusiveRefCntPtr<FileManager> FileMgr =
      new FileManager(FileSystemOptions(), FS);
  llvm::IntrusiveRefCntPtr<SourceManager> SourceMgr =
      new SourceManager(*Diags, *FileMgr);
  /// Contains last result of calling recordTokens().
  TokenBuffer Buffer;
  /// Contains options from last run of recordTokens().
  LangOptions LangOpts;
};

TEST_F(TokenBufferTest, RawMode) {
  EXPECT_THAT(tokenize("int main() {}").tokens(),
              ElementsAre(Kind(tok::kw_int), IsIdent("main"),
                          Kind(tok::l_paren), Kind(tok::r_paren),
                          Kind(tok::l_brace), Kind(tok::r_brace),
                          Kind(tok::eof)));
  // Comments are ignored for now.
  EXPECT_THAT(tokenize("/* foo */int a; // more comments").tokens(),
              ElementsAre(Kind(tok::kw_int), IsIdent("a"), Kind(tok::semi),
                          Kind(tok::eof)));
}

TEST_F(TokenBufferTest, Basic) {
  recordTokens("int main() {}");
  EXPECT_THAT(Buffer.tokens(),
              ElementsAre(Kind(tok::kw_int), IsIdent("main"),
                          Kind(tok::l_paren), Kind(tok::r_paren),
                          Kind(tok::l_brace), Kind(tok::r_brace),
                          Kind(tok::eof)));
  // All kinds of whitespace are ignored.
  recordTokens("\t\n  int\t\n  main\t\n  (\t\n  )\t\n{\t\n  }\t\n");
  EXPECT_THAT(Buffer.tokens(),
              ElementsAre(Kind(tok::kw_int), IsIdent("main"),
                          Kind(tok::l_paren), Kind(tok::r_paren),
                          Kind(tok::l_brace), Kind(tok::r_brace),
                          Kind(tok::eof)));

  llvm::Annotations Code(R"cpp(
    $r1[[int]] $r2[[a]] $r3[[=]] $r4[["foo bar baz"]] $r5[[;]]
  )cpp");
  recordTokens(Code.code());
  EXPECT_THAT(
      Buffer.tokens(),
      ElementsAre(AllOf(Kind(tok::kw_int), RangeIs(Code.range("r1"))),
                  AllOf(Kind(tok::identifier), RangeIs(Code.range("r2"))),
                  AllOf(Kind(tok::equal), RangeIs(Code.range("r3"))),
                  AllOf(Kind(tok::string_literal), RangeIs(Code.range("r4"))),
                  AllOf(Kind(tok::semi), RangeIs(Code.range("r5"))),
                  Kind(tok::eof)));
}

TEST_F(TokenBufferTest, MacroDirectives) {
  // Macro directives are not stored anywhere at the moment.
  recordTokens(R"cpp(
    #define FOO a
    #include "unresolved_file.h"
    #undef FOO
    #ifdef X
    #else
    #endif
    #ifndef Y
    #endif
    #if 1
    #elif 2
    #else
    #endif
    #pragma once
    #pragma something lalala

    int a;
  )cpp");

  checkTokens("int a;");
  EXPECT_THAT(Buffer.expansions(), IsEmpty());
  EXPECT_THAT(Buffer.macroTokens(), IsEmpty());
}

TEST_F(TokenBufferTest, MacroExpansions) {
  // A simple macro definition and expansion.
  llvm::Annotations Code(R"cpp(
    #define INT int const
    [[INT]] a;
    )cpp");
  recordTokens(Code.code());

  checkTokens("int const a;");
  checkExpansions({{"INT", "int const", Code.range()}});

  // A simple functional macro invocation.
  Code = llvm::Annotations(R"cpp(
    #define INT(a) const int
    [[INT(10+10)]] a;
    )cpp");
  recordTokens(Code.code());

  checkTokens("const int a;");
  checkExpansions({{"INT(10+10)", "const int", Code.range()}});

  // Recursive macro expansions.
  Code = llvm::Annotations(R"cpp(
    #define ID(X) X
    #define INT int const
    [[ID(ID(INT))]] a;
  )cpp");
  recordTokens(Code.code());

  checkTokens("int const a;");
  checkExpansions({{"ID(ID(INT))", "int const", Code.range()}});

  // Empty macro expansions.
  Code = llvm::Annotations(R"cpp(
    #define EMPTY
    #define EMPTY_FUNC(X)
    $m[[EMPTY]]
    $f[[EMPTY_FUNC(1+2+3)]]
  )cpp");
  recordTokens(Code.code());

  checkTokens("");
  checkExpansions({{"EMPTY", "", Code.range("m")},
                   {"EMPTY_FUNC(1+2+3)", "", Code.range("f")}});
}

TEST_F(TokenBufferTest, SpecialTokens) {
  // Tokens coming from concatenations.
  recordTokens(R"cpp(
    #define CONCAT(a, b) a ## b
    int a = CONCAT(1, 2);
  )cpp");
  checkTokens("int a = 12;");
  // Multi-line tokens with slashes at the end.
  recordTokens("i\\\nn\\\nt");
  EXPECT_THAT(Buffer.tokens(),
              ElementsAre(AllOf(Kind(tok::kw_int), HasText("i\\\nn\\\nt")),
                          Kind(tok::eof)));
  // FIXME: test tokens with digraphs and UCN identifiers.
}

TEST_F(TokenBufferTest, LateBoundTokens) {
  // The parser eventually breaks the first '>>' into two tokens ('>' and '>'),
  // but we chooses to record them as a single token (for now).
  llvm::Annotations Code(R"cpp(
    template <class T>
    struct foo { int a; };
    int bar = foo<foo<int$br[[>>]]().a;
    int baz = 10 $op[[>>]] 2;
  )cpp");
  recordTokens(Code.code());
  EXPECT_THAT(std::vector<syntax::Token>(Buffer.tokens()),
              AllOf(Contains(AllOf(Kind(tok::greatergreater),
                                   RangeIs(Code.range("br")))),
                    Contains(AllOf(Kind(tok::greatergreater),
                                   RangeIs(Code.range("op"))))));
}

TEST_F(TokenBufferTest, DelayedParsing) {
  llvm::StringLiteral Code = R"cpp(
    struct Foo {
      int method() {
        // Parser will visit method bodies and initializers multiple time, but
        // TokenBuffer should only record the first walk over the tokens;
        return 100;
      }
      int a = 10;
      int b = 20;

      struct Subclass {
        void foo() {
          Foo().method();
        }
      };
    };
  )cpp";
  recordTokens(Code);
  // Checks that lexing in raw mode produces the same results, hence we're not
  // recording any tokens twice and the order is the same.
  checkTokens(Code);
}

TEST_F(TokenBufferTest, Offsets) {
  llvm::Annotations Code("");
  auto OfKind = [this](tok::TokenKind K) {
    auto It = llvm::find_if(
        Buffer.tokens(), [K](const syntax::Token &T) { return T.kind() == K; });
    assert(It != Buffer.tokens().end());
    return It;
  };
  auto Range = [&Code](llvm::StringRef Name) {
    auto R = Code.range(Name);
    return std::pair<unsigned, unsigned>(R.Begin, R.End);
  };

  Code = llvm::Annotations(R"cpp(
    $all[[int $a[[a]] = $numbers[[100 + 200]];]]
  )cpp");

  recordTokens(Code.code());
  EXPECT_EQ(Buffer.toOffsetRange(OfKind(tok::kw_int),
                                 std::next(OfKind(tok::semi)), *SourceMgr),
            Range("all"));
  EXPECT_EQ(Buffer.toOffsetRange(OfKind(tok::identifier),
                                 std::next(OfKind(tok::identifier)),
                                 *SourceMgr),
            Range("a"));
  EXPECT_EQ(Buffer.toOffsetRange(OfKind(tok::numeric_constant),
                                 OfKind(tok::semi), *SourceMgr),
            Range("numbers"));

  Code = llvm::Annotations(R"cpp(
    #define ID(a) a
    #define NUMBERS 100 + 200
    $all[[ID(int) $a[[ID(a)]] = $numbers[[NUMBERS]];]]
  )cpp");
  recordTokens(Code.code());
  EXPECT_EQ(Buffer.toOffsetRange(OfKind(tok::kw_int),
                                 std::next(OfKind(tok::semi)), *SourceMgr),
            Range("all"));
  EXPECT_EQ(*Buffer.toOffsetRange(OfKind(tok::identifier),
                                 std::next(OfKind(tok::identifier)),
                                 *SourceMgr),
            Range("a"));
  EXPECT_EQ(*Buffer.toOffsetRange(OfKind(tok::numeric_constant),
                                 OfKind(tok::semi), *SourceMgr),
            Range("numbers"));
  // Ranges not fully covering macro expansions should fail.
  EXPECT_EQ(Buffer.toOffsetRange(OfKind(tok::numeric_constant),
                                 std::next(OfKind(tok::numeric_constant)),
                                 *SourceMgr),
            llvm::None);
  EXPECT_EQ(Buffer.toOffsetRange(OfKind(tok::plus),
                                 std::next(OfKind(tok::plus)), *SourceMgr),
            llvm::None);
}

} // namespace
