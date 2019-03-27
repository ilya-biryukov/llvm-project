//===- TokenBuffer.cpp - store tokens of preprocessed files ---*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "clang/Tooling/Syntax/TokenBuffer.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TokenKinds.def"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/Token.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/FormatVariadic.h"
#include <iterator>

using namespace clang;
using namespace clang::syntax;

syntax::Token::Token(const clang::Token &T)
    : Token(T.getLocation(), T.getLength(), T.getKind()) {
  assert(!T.isAnnotation());
}
llvm::StringRef syntax::Token::text(const SourceManager &SM) const {
  bool Invalid = false;
  const char *Start = SM.getCharacterData(location(), &Invalid);
  assert(!Invalid);
  return llvm::StringRef(Start, length());
}

TokenBuffer syntax::tokenize(FileID FID, const SourceManager &SM,
                             const LangOptions &LO) {
  std::vector<syntax::Token> Tokens;
  IdentifierTable Identifiers(LO);
  auto AddToken = [&](clang::Token T) {
    if (T.getKind() == tok::raw_identifier && !T.needsCleaning() &&
        !T.hasUCN()) { // FIXME: support needsCleaning and hasUCN cases.
      clang::IdentifierInfo &II = Identifiers.get(T.getRawIdentifier());
      T.setIdentifierInfo(&II);
      T.setKind(II.getTokenID());
    }
    Tokens.push_back(syntax::Token(T));
  };

  Lexer L(FID, SM.getBuffer(FID), SM, LO);

  clang::Token T;
  while (!L.LexFromRawLexer(T))
    AddToken(T);
  AddToken(T);

  return TokenBuffer(std::move(Tokens));
}

class TokenCollector::Callbacks : public PPCallbacks {
public:
  Callbacks(const SourceManager &SM, const LangOptions &LO, TokenBuffer &Result)
      : Result(Result), SM(SM), LO(LO) {}

  void FileChanged(SourceLocation Loc, FileChangeReason Reason,
                   SrcMgr::CharacteristicKind FileType,
                   FileID PrevFID) override {
    assert(Loc.isFileID());
    InsideMainFile = SM.getFileID(Loc) == SM.getMainFileID();
    flushCurrentExpansion();
  }

  void MacroDefined(const clang::Token &MacroNameTok,
                    const MacroDirective *MD) override {
    flushCurrentExpansion();
    handleMacroDirective(MacroNameTok.getLocation(), /*AnchorDiff=*/2);
  }

  void MacroUndefined(const clang::Token &MacroNameTok,
                      const MacroDefinition &MD,
                      const MacroDirective *Undef) override {
    flushCurrentExpansion();
    handleMacroDirective(MacroNameTok.getLocation(), /*AnchorDiff=*/2);
  }

  void InclusionDirective(SourceLocation HashLoc,
                          const clang::Token &IncludeTok, StringRef FileName,
                          bool IsAngled, CharSourceRange FilenameRange,
                          const FileEntry *File, StringRef SearchPath,
                          StringRef RelativePath, const Module *Imported,
                          SrcMgr::CharacteristicKind FileType) override {
    flushCurrentExpansion();
    handleMacroDirective(IncludeTok.getLocation(), /*AnchorDiff=*/1);
  }

  void If(SourceLocation Loc, SourceRange ConditionRange,
          ConditionValueKind ConditionValue) override {
    flushCurrentExpansion();
    handleMacroDirective(Loc, /*AnchorDiff=*/1);
  }

  void Elif(SourceLocation Loc, SourceRange ConditionRange,
            ConditionValueKind ConditionValue, SourceLocation IfLoc) override {
    flushCurrentExpansion();
    handleMacroDirective(Loc, /*AnchorDiff=*/1);
  }

  void Ifdef(SourceLocation Loc, const clang::Token &MacroNameTok,
             const MacroDefinition &MD) override {
    flushCurrentExpansion();
    handleMacroDirective(Loc, /*AnchorDiff=*/1);
  }

  void Ifndef(SourceLocation Loc, const clang::Token &MacroNameTok,
              const MacroDefinition &MD) override {
    flushCurrentExpansion();
    handleMacroDirective(Loc, /*AnchorDiff=*/1);
  }

  void Else(SourceLocation Loc, SourceLocation IfLoc) override {
    flushCurrentExpansion(Loc);
    handleMacroDirective(Loc, /*AnchorDiff=*/1);
  }

  void Endif(SourceLocation Loc, SourceLocation IfLoc) override {
    flushCurrentExpansion();
    handleMacroDirective(Loc, /*AnchorDiff=*/1);
  }

  // FIXME: missing moduleImport(), Ident(), ...

  void PragmaDirective(SourceLocation Loc,
                       PragmaIntroducerKind Introducer) override {
    if (!InsideMainFile)
      return;
    assert(PragmaStart.isInvalid() && "Recursive #pragma directives?");
    PragmaStart = Loc;
  }

  void tokenLexed(const clang::Token &T) {
    if (!InsideMainFile)
      return;
    auto L = T.getLocation();
    assert(L.isValid());

    // Parser sometimes goes through the same tokens again, we are only
    // interested in the initial iteration.
    if (!Result.Tokens.empty() &&
        !SM.isBeforeInTranslationUnit(Result.Tokens.back().location(), L))
      return;
    flushCurrentExpansion(L);

    if (ExpansionStart.isValid() && SM.getExpansionLoc(L) != ExpansionStart) {
      // There are intermediate macro argument expansions. Skip them, they will
      // be reported again later.
      return;
    }

    DEBUG_WITH_TYPE("collect-tokens",
                    llvm::dbgs() << llvm::formatv(
                        "$[token], name - {0}, length - {1}, spelling - {2}\n",
                        tok::getTokenName(T.getKind()), T.getLength(),
                        Lexer::getSpelling(T, SM, LO)));
    Result.Tokens.push_back(syntax::Token(T));
    assert(Result.Tokens.back().location().isValid());

    // Process the end of #pragma directive.
    if (PragmaStart.isValid() && T.getKind() == tok::eod) {
      handleMacroDirective(PragmaStart, /*AnchorDiff=*/0);
      PragmaStart = SourceLocation();
      return;
    }
  }

  void MacroExpands(const clang::Token &MacroNameTok, const MacroDefinition &MD,
                    SourceRange Range, const MacroArgs *Args) override {
    if (!InsideMainFile)
      return;

    auto MacroNameLoc = MacroNameTok.getLocation();
    flushCurrentExpansion(MacroNameLoc);

    // Note that MacroNameTok was not reported yet.
    auto ExpansionStart =
        std::find_if(Result.Tokens.rbegin(), Result.Tokens.rend(),
                     [&](const syntax::Token &T) {
                       return SM.isBeforeInTranslationUnit(T.location(),
                                                           MacroNameLoc);
                     })
            .base();
    if (ExpansionFile.isValid()) {
      // This is a recursive macro expansion, so we do not need to record it.
      DEBUG_WITH_TYPE("collect-tokens",
                      llvm::dbgs() << llvm::formatv(
                          "$[macro-expands] dropping {0} macro tokens\n",
                          std::distance(ExpansionStart, Result.Tokens.end())));
      Result.Tokens.erase(ExpansionStart, Result.Tokens.end());
      return;
    }
    // This is a new top-level macro expansion, record it.
    MacroExpansion MC;
    MC.BeginFileToken = Result.MacroTokens.size();
    MC.EndFileToken =
        MC.BeginFileToken + (Result.Tokens.end() - ExpansionStart) + 1;
    // Store the macro name and macro arguments, they are used when calculating
    // textual.
    Result.MacroTokens.push_back(syntax::Token(MacroNameTok));
    for (auto &T : llvm::make_range(ExpansionStart, Result.Tokens.end()))
      Result.MacroTokens.push_back(T);
    // Macro call tokens are not part of the expanded tokens, so remove them.
    DEBUG_WITH_TYPE("collect-tokens",
                    llvm::dbgs() << llvm::formatv(
                        "$[macro-expands] dropping {0} macro tokens\n",
                        std::distance(ExpansionStart, Result.Tokens.end())));
    Result.Tokens.erase(ExpansionStart, Result.Tokens.end());

    MC.BeginExpansionToken = Result.Tokens.size();
    // MC.EndExpansionToken is filled after the expansion finishes.
    Result.Expansions.push_back(MC);
    // We need to record where expansion ends in order to track it properly.
    std::tie(ExpansionFile, ExpansionEndOffset) =
        SM.getDecomposedLoc(Range.getEnd());
    this->ExpansionStart = Range.getBegin();
  }

private:
  void handleMacroDirective(SourceLocation Anchor, int AnchorOffset) {
    if (!InsideMainFile)
      return;

    flushCurrentExpansion(Anchor);

    assert(!Result.Tokens.empty());
    assert(Result.Tokens.back().kind() == tok::eod);
    auto MacroStart = std::find_if(Result.Tokens.rbegin(), Result.Tokens.rend(),
                                   [&](const syntax::Token &T) {
                                     return T.location() == Anchor;
                                   })
                          .base();
    // MacroStart now points a few tokens after the start of the macro, e.g.
    //   # define MACRO ^...
    //   # include ^...
    // we want to move it to point to 'define' or 'include', respectively.
    //   # ^define MACRO ...
    //   # ^include ...
    assert(std::distance(Result.Tokens.begin(), MacroStart) >= AnchorOffset);
    std::advance(MacroStart, -AnchorOffset);

    DEBUG_WITH_TYPE("collect-tokens",
                    llvm::dbgs() << llvm::formatv(
                        "$[pp-directive] dropping {0} macro directive tokens\n",
                        std::distance(MacroStart, Result.Tokens.end())));
    Result.Tokens.erase(MacroStart, Result.Tokens.end());
  }

private:
  void flushCurrentExpansion() {
    if (!ExpansionFile.isValid())
      return;
    assert(!Result.Expansions.empty());
    assert(Result.Expansions.back().EndExpansionToken == 0);
    Result.Expansions.back().EndExpansionToken = Result.Tokens.size();

    ExpansionFile = FileID();
    ExpansionStart = SourceLocation();
    ExpansionEndOffset = 0;
  }

  void flushCurrentExpansion(SourceLocation L) {
    assert(L.isValid());
    if (!ExpansionFile.isValid())
      return;
    FileID File;
    unsigned Offset;
    std::tie(File, Offset) = SM.getDecomposedLoc(L);
    if (File != ExpansionFile || Offset <= ExpansionEndOffset)
      return;
    // Check we are not inside the current macro arguments.
    flushCurrentExpansion();
  }

  bool InsideMainFile = false;
  // The start location of the currently processed #pragma directive.
  SourceLocation PragmaStart;
  /// When valid, the range of the last active top-level macro expansion.
  FileID ExpansionFile;
  SourceLocation ExpansionStart;
  unsigned ExpansionEndOffset = 0;
  TokenBuffer &Result;
  const SourceManager &SM;
  const LangOptions &LO;
};

llvm::ArrayRef<syntax::Token>
MacroExpansion::tokens(const TokenBuffer &B) const {
  return B.tokens().slice(BeginExpansionToken,
                          EndExpansionToken - BeginExpansionToken);
}

llvm::ArrayRef<syntax::Token>
MacroExpansion::macroTokens(const TokenBuffer &B) const {
  return B.macroTokens().slice(BeginFileToken, EndFileToken - BeginFileToken);
}

std::pair<unsigned, unsigned>
MacroExpansion::macroRange(const TokenBuffer &B,
                           const SourceManager &SM) const {
  auto M = macroTokens(B);
  return {SM.getFileOffset(M.front().location()),
          SM.getFileOffset(M.back().endLocation())};
}

TokenBuffer::TokenBuffer(std::vector<syntax::Token> Tokens)
    : Tokens(std::move(Tokens)) {
#ifndef NDEBUG
  for (const auto &T : this->Tokens)
    assert(T.location().isFileID());
#endif
}

TokenCollector::TokenCollector(Preprocessor &PP) {
  auto CBOwner = llvm::make_unique<Callbacks>(PP.getSourceManager(),
                                              PP.getLangOpts(), Tokens);
  auto *CB = CBOwner.get();

  PP.addPPCallbacks(std::move(CBOwner));
  PP.setTokenWatcher([CB](const clang::Token &T) { CB->tokenLexed(T); });
}

TokenBuffer TokenCollector::consume() && { return std::move(Tokens); }

llvm::Optional<std::pair<unsigned, unsigned>>
TokenBuffer::toOffsetRange(const Token *Begin, const Token *End,
                           const SourceManager &SM) const {
  assert(Begin < End);
  unsigned BeginIndex = Begin - Tokens.data();
  unsigned EndIndex = End - Tokens.data();

  // Find the first macro call that intersects with our range.
  auto FirstCall =
      std::upper_bound(Expansions.begin(), Expansions.end(), BeginIndex,
                       [](unsigned L, const MacroExpansion &R) {
                         return L < R.BeginExpansionToken;
                       });
  if (FirstCall != Expansions.begin()) {
    --FirstCall;
    if (FirstCall->EndExpansionToken <= BeginIndex)
      FirstCall = Expansions.end();
  } else {
    FirstCall = Expansions.end();
  }
  // Find the last macro call that intersects with our range.
  auto LastCall =
      std::lower_bound(Expansions.begin(), Expansions.end(), EndIndex,
                       [](const MacroExpansion &L, unsigned R) {
                         return L.EndExpansionToken < R;
                       });
  if (LastCall != Expansions.end() && EndIndex <= LastCall->BeginExpansionToken)
    LastCall = Expansions.end();
  // Only allow changes that involve the whole macro calls, disallow anything
  // that changes macros in between.
  // FIXME: also allow changes uniquely mapping to macro arguments.
  assert(FirstCall == Expansions.end() || LastCall == Expansions.end() ||
         FirstCall <= LastCall);

  // Check the first macro call is fully-covered.
  if (FirstCall != Expansions.end() &&
      (FirstCall->BeginExpansionToken < BeginIndex ||
       EndIndex < FirstCall->EndExpansionToken)) {
    return llvm::None;
  }
  // Check the last macro call is fully-covered.
  if (LastCall != Expansions.end() &&
      (LastCall->BeginExpansionToken < BeginIndex ||
       EndIndex < LastCall->EndExpansionToken)) {
    return llvm::None;
  }

  unsigned BeginOffset =
      SM.getFileOffset(FirstCall != Expansions.end()
                           ? FirstCall->macroTokens(*this).front().location()
                           : Begin->location());
  unsigned EndOffset =
      SM.getFileOffset(LastCall != Expansions.end()
                           ? LastCall->macroTokens(*this).back().endLocation()
                           : std::prev(End)->endLocation());
  return std::make_pair(BeginOffset, EndOffset);
}

const MacroExpansion *TokenBuffer::findMacroCall(const Token *Begin) const {
  unsigned Index = Begin - Tokens.data();
  auto It = std::lower_bound(Expansions.begin(), Expansions.end(), Index,
                             [](const MacroExpansion &L, unsigned R) {
                               return L.BeginExpansionToken < R;
                             });
  return It != Expansions.end() && It->BeginExpansionToken == Index ? &*It
                                                                    : nullptr;
}
