//===- TokenBuffer.h - store tokens of preprocessed files -----*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLING_SYNTAX_TOKEN_BUFFER_H
#define LLVM_CLANG_TOOLING_SYNTAX_TOKEN_BUFFER_H

#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TokenKinds.def"
#include "clang/Basic/TokenKinds.h"
#include "clang/Lex/Token.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include <cstdint>

namespace clang {
class Preprocessor;

namespace syntax {
class TokenBuffer;

/// A token coming directly from a file or a macro expansion. Has just enough
/// information to locate the token in the source code.
class Token {
public:
  Token() = default;
  Token(SourceLocation Location, unsigned Length, tok::TokenKind Kind)
      : Location(Location), Length(Length), Kind(Kind) {}
  /// EXPECTS: clang::Token is not an annotation token.
  explicit Token(const clang::Token &T);

  tok::TokenKind kind() const { return Kind; }
  SourceLocation location() const { return Location; }
  SourceLocation endLocation() const {
    return Location.getLocWithOffset(Length);
  }
  unsigned length() const { return Length; }

  /// Get the substring covered by the token. Note that will include all
  /// digraphs, newline continuations, etc. E.g. 'int' and
  ///    in\
  ///    t
  /// both same kind tok::kw_int, but results of getText are different.
  llvm::StringRef text(const SourceManager &SM) const;

private:
  SourceLocation Location;
  unsigned Length = 0;
  tok::TokenKind Kind = tok::NUM_TOKENS;
};

static_assert(sizeof(Token) <= 16, "Token is unresonably large");

/// A top-level macro expansion inside a file.
class MacroExpansion {
public:
  /// The tokens obtained after expansion.
  llvm::ArrayRef<syntax::Token> tokens(const TokenBuffer &B) const;
  /// These cover the name and arguments of a macro (if any), incluging the
  /// parentheses around macro arguments.
  llvm::ArrayRef<syntax::Token> macroTokens(const TokenBuffer &B) const;
  /// Range of offsets covering the name of a macro expansion or the name and
  /// arguments of a functional macro invocation.
  std::pair<unsigned, unsigned> macroRange(const TokenBuffer &B,
                                           const SourceManager &SM) const;

private:
  friend class TokenCollector;
  friend class TokenBuffer;
  unsigned BeginExpansionToken = 0;
  unsigned EndExpansionToken = 0;
  unsigned BeginFileToken = 0;
  unsigned EndFileToken = 0;
};

/// A list of tokens obtained by lexing and preprocessing a text buffer and a
/// set of helpers to allow mapping the tokens after preprocessing to the
/// corresponding code written in a file. TokenBuffer has information about two
/// token streams:
///    1. tokens produced by the preprocessor, i.e. after all macro expansions,
///    2. pre-expansion tokens that corresponds to the source code of a file.
/// The tokens for (1) are stored directly and can be accessed with the tokens()
/// method. However, some of these tokens may come from macro expansions and so
/// they don't correspond directly to any text in a file, e.g.
///
///     #define FOO 10
///     int a = FOO;  // no token '10' in the file, just 'FOO'
///
/// For these tokens, TokenBuffer allows to obtain the macro name and macro
/// arguments that were used to produce the expansion with the 'toOffsetRange()'
/// method.
/// There are two ways to build a TokenBuffer:
///   1. If you are running a clang frontend invocation, use the TokenCollector
///      class,
///   2. if you only need to lex a file, use the tokenize() helper.
class TokenBuffer {
public:
  TokenBuffer() = default;
  // Assumes no macro expansions have taken place.
  TokenBuffer(std::vector<syntax::Token> Tokens);

  /// All tokens from the result of preprocessor expansion, i.e. the list of
  /// tokens produced by the preprocessor. Source locations in the clang AST
  /// should always point into any of these nodes.
  llvm::ArrayRef<syntax::Token> tokens() const { return Tokens; }
  /// Attempt the map a range of expanded tokens into a continuous substring of
  /// the original source file. The tranformation may not be possible if the
  /// range requires changing the macro expansions.
  llvm::Optional<std::pair<unsigned, unsigned>>
  toOffsetRange(const Token *Begin, const Token *End,
                const SourceManager &SM) const;

  /// All top-level macro expansions from the corresponding file. Includes
  /// functional macro invocation and expansion of macro identifiers. E.g would
  /// contain 3 entries for the following code:
  ///     #define FOO 2*5
  ///     #define BAR(a,b) a+b+FOO
  ///     BAR(FOO, FOO) // #1
  ///     int a = FOO; // #2
  ///     int b = BAR(a, BAR(6, FOO)); // #3
  /// Note that neither expansions inside macro arguments (e.g. 'FOO' in
  /// 'BAR(FOO, FOO)') nor recursive macro expansions are present in the
  /// result.
  llvm::ArrayRef<MacroExpansion> expansions() const { return Expansions; }
  /// Tokens of macro directives and top-level macro expansions. These are not
  /// part of the expanded token stream, but they fill the gaps for the file.
  /// Here is an example:
  ///     #define DECL(name) int name = 10
  ///     DECL(a);
  /// For the input above, we would get tokens() = {"int", "a", "=", "10", ";"}
  /// and macroTokens() = {"DECL", "(", "a", ")"}.
  llvm::ArrayRef<syntax::Token> macroTokens() const { return MacroTokens; }

private:
  friend class TokenCollector;
  friend class MacroExpansion;
  /// Expanded tokens, the ASTs are built on top of these. Some of the tokens
  /// have file locations and can be used to obtain the file offsets directly.
  std::vector<syntax::Token> Tokens;
  /// Tokens forming top-level macro expansions, i.e. all macro names and macro
  /// arguments.
  std::vector<syntax::Token> MacroTokens;
  /// A list of top-level macro expansions inside a particular file.
  std::vector<MacroExpansion> Expansions;
};

/// Lex the text buffer, corresponding to \p FID, in raw mode and record the
/// resulting tokens. Does minimal post-processing on raw identifiers, setting
/// their corresponding token kind. This is a very low-level function, most
/// users should prefer to use TokenCollector. Lexing in raw mode produces
/// wildly different results from what one might expect when running a C++
/// frontend, e.g. preprocessor does not run at all.
TokenBuffer tokenize(FileID FID, const SourceManager &SM,
                     const LangOptions &LO);

/// Collects tokens for the main file while running the frontend action. An
/// instance of this object should be created on
/// FrontendAction::BeginSourceFile() and the results should be consumed after
/// FrontendAction::Execute() finishes.
class TokenCollector {
public:
  /// Adds the hooks to collect the tokens. Should be called before the
  /// preprocessing starts, i.e. as a part of BeginSourceFile() or
  /// CreateASTConsumer().
  TokenCollector(Preprocessor &P);

  /// Consumes the result. Should be called after preprocessing is finished,
  /// i.e. after running Execute().
  TokenBuffer consume() &&;

private:
  class Callbacks;
  TokenBuffer Tokens;
};

} // namespace syntax
} // namespace clang

#endif
