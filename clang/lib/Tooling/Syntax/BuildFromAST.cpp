//===- BuildFromAST.cpp ---------------------------------------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Syntax/Syntax.h"

using namespace clang;

namespace {
/// A helper class for constructing the syntax tree bottom-up while traversing a
/// clang AST.
class TreeBuilder {
public:
  TreeBuilder(syntax::Corpus &Corpus) : Corpus(Corpus) {}

  syntax::Corpus &corpus() { return Corpus; }

  /// Populate children for \p New, assuming it covers the source range
  /// specified by \p Begin and \p End.
  void learnNode(SourceLocation Begin, SourceLocation End,
                 syntax::TreeNode *New) {
    learnNodeImpl(Begin, End, New, /*TweakChildren=*/nullptr);
  }

  /// Same as learnNode, but wraps child expressions into
  /// syntax::ExpressionStmt. Note that the clang AST does not have a node,
  /// corresponding to ExpressionStmt.
  void learnCompoundStatement(SourceLocation Begin, SourceLocation End,
                              syntax::CompoundStmt *New);

  /// Finish building the tree and create a root node. No further calls to
  /// learn* methods are allowed.
  void learnRoot();

  /// Consume the root node.
  syntax::TranslationUnit *root() && {
    assert(Root);
    assert(NodesInProgress.empty());
    return Root;
  }

private:
  struct RangedNode {
    RangedNode(llvm::ArrayRef<syntax::Token> Tokens, syntax::TreeNode *Node)
        : Tokens(Tokens), Node(Node) {}

    llvm::ArrayRef<syntax::Token> Tokens;
    syntax::TreeNode *Node;
  };
  const syntax::Token *findToken(SourceLocation TokLoc) const;
  template <class TNode, class... Args> TNode *construct(Args &&... args);

  void learnNodeImpl(
      SourceLocation Begin, SourceLocation End, syntax::TreeNode *New,
      llvm::function_ref<void(std::vector<RangedNode>::iterator FirstChild)>
          TweakChildren);

  syntax::Corpus &Corpus;
  std::vector<RangedNode> NodesInProgress;
  syntax::TranslationUnit *Root = nullptr;
};

class BuildTreeVisitor : public RecursiveASTVisitor<BuildTreeVisitor> {
public:
  explicit BuildTreeVisitor(ASTContext &Ctx, TreeBuilder &Builder)
      : Builder(Builder), LangOpts(Ctx.getLangOpts()) {}

  bool shouldTraversePostOrder() const { return true; }

  bool TraverseDecl(Decl *D) {
    if (!D || isa<TranslationUnitDecl>(D))
      return RecursiveASTVisitor::TraverseDecl(D);
    auto &SM = Builder.corpus().sourceManager();
    // We only build AST for the main file for the purpose of the prototype.
    if (!SM.isWrittenInMainFile(SM.getExpansionLoc(D->getBeginLoc())) ||
        !SM.isWrittenInMainFile(SM.getExpansionLoc(D->getEndLoc())))
      return true;
    return RecursiveASTVisitor::TraverseDecl(D);
  }

  // Some nodes mess up our post-order traversal, ignore those for now.
  // FIXME: figure what the problems are in the traversal and fix them.
  bool TraverseInitListExpr(InitListExpr *E) { return true; }
  bool TraverseCXXForRangeStmt(CXXForRangeStmt *S) { return true; }
  bool TraverseCXXOperatorCallExpr(CXXOperatorCallExpr *C) { return true; }

  bool WalkUpFromTranslationUnitDecl(TranslationUnitDecl *TU) {
    Builder.learnRoot();
    // Note that we deliberately do not call VisitDecl.
    return true;
  }

  bool VisitStmt(clang::Stmt *S) {
    Builder.learnNode(S->getBeginLoc(), S->getEndLoc(),
                      corpus().construct<syntax::RecoveryNode>());
    return true;
  }

#define TRIVIAL_NODE(ASTNode, SynNode)                                         \
  bool WalkUpFrom##ASTNode(clang::ASTNode *N) {                                \
    return WalkUpFromTrivial<syntax::SynNode, clang::ASTNode>(N);              \
  }

  // Expressions.
  TRIVIAL_NODE(Expr, UnknownExpr);
  TRIVIAL_NODE(UnaryOperator, UnaryExpr);
  TRIVIAL_NODE(BinaryOperator, BinaryExpr);
  TRIVIAL_NODE(DeclRefExpr, ReferenceExpr);
  TRIVIAL_NODE(ParenExpr, ParenExpr);
  TRIVIAL_NODE(IntegerLiteral, LiteralExpr);
  TRIVIAL_NODE(FixedPointLiteral, LiteralExpr);
  TRIVIAL_NODE(CharacterLiteral, LiteralExpr);
  TRIVIAL_NODE(FloatingLiteral, LiteralExpr);
  TRIVIAL_NODE(ImaginaryLiteral, LiteralExpr);
  TRIVIAL_NODE(StringLiteral, LiteralExpr);
  TRIVIAL_NODE(PredefinedExpr, PredefinedExpr);

  // Do not create nodes for implicit semantic nodes, e.g. implicit cast, etc.
  bool WalkUpFromCXXThisExpr(CXXThisExpr *T) {
    if (T->isImplicit())
      return true; // skip implicit code.
    return RecursiveASTVisitor::WalkUpFromCXXThisExpr(T);
  }
  bool WalkUpFromCXXDefaultArgExpr(CXXDefaultArgExpr *E) { return true; }
  bool WalkUpFromCXXConstructExpr(CXXConstructExpr *E) { return true; }
  bool WalkUpFromImplicitCastExpr(ImplicitCastExpr *E) { return true; }
  bool WalkUpFromExprWithCleanups(ExprWithCleanups *E) { return true; }
  bool WalkUpFromCXXBindTemporaryExpr(CXXBindTemporaryExpr *E) { return true; }
  bool WalkUpFromMaterializeTemporaryExpr(MaterializeTemporaryExpr *E) {
    return true;
  }

  template <class ASTStmt> bool WalkUpFromLeafStatement(Stmt *S) {
    auto End = S->getEndLoc();
    if (auto Tok =
            Lexer::findNextToken(End, corpus().sourceManager(), LangOpts)) {
      if (Tok->getKind() == tok::semi)
        End = Tok->getLocation();
    }
    Builder.learnNode(S->getBeginLoc(), End, corpus().construct<ASTStmt>());
    return true;
  }

// Statements.
/// Leaf statements consume the trailing semicolon.
#define LEAF_STMT(ASTStmt, SynStmt)                                            \
  bool WalkUpFrom##ASTNode(clang::ASTStmt *N) {                                \
    return WalkUpFromTrivial<syntax::SynStmt, clang::ASTStmt>(N);              \
  }
/// Composite statements do not have trailing semicolons.
#define COMPOSITE_STMT(ASTStmt, SynStmt) TRIVIAL_NODE(ASTStmt, SynStmt)

  LEAF_STMT(Stmt, UnknownStmt);
  COMPOSITE_STMT(IfStmt, IfStmt);
  COMPOSITE_STMT(ForStmt, ForStmt);
  COMPOSITE_STMT(WhileStmt, WhileStmt);
  COMPOSITE_STMT(DoStmt, DoStmt);
  COMPOSITE_STMT(CXXForRangeStmt, RangeBasedForStmt)
  LEAF_STMT(ReturnStmt, ReturnStmt);

  bool WalkUpFromCompoundStmt(CompoundStmt *S) {
    // Compound statement is special, we wrap all expressions into an
    // ExpressionStmt node.
    Builder.learnCompoundStatement(S->getBeginLoc(), S->getEndLoc(),
                                   corpus().construct<syntax::CompoundStmt>());
    return true;
  }

#undef TRIVIAL_NODE

private:
  template <class SynNode, class ASTNode> bool WalkUpFromTrivial(ASTNode *N) {
    Builder.learnNode(N->getBeginLoc(), N->getEndLoc(),
                      corpus().construct<SynNode>());
    return true;
  }

  /// A small helper to save some typing.
  syntax::Corpus &corpus() { return Builder.corpus(); }

  TreeBuilder &Builder;
  const LangOptions &LangOpts;
};

void TreeBuilder::learnCompoundStatement(SourceLocation Begin,
                                         SourceLocation End,
                                         syntax::CompoundStmt *New) {
  learnNodeImpl(
      Begin, End, New, [this](std::vector<RangedNode>::iterator FirstChild) {
        for (auto &N : NodesInProgress) {
          assert(N.Node->parent() == nullptr);
        }
        // Wrap child expressions into expression statements.
        auto &C = corpus();
        auto Tokens = C.mainFile().tokens();
        for (auto It = FirstChild; It != NodesInProgress.end(); ++It) {
          if (!llvm::isa<syntax::Expr>(It->Node))
            continue;
          auto *S = construct<syntax::ExpressionStmt>();
          S->addChild(C, It->Node);
          // Reassign the node.
          It->Node = S;
          // Add trailing semicolon, if any.
          auto TrailingSemi = It->Tokens.end();
          if (TrailingSemi == Tokens.end())
            continue;
          if (TrailingSemi->kind() != tok::semi)
            continue;
          assert((std::next(It) == NodesInProgress.end() ||
                  TrailingSemi < std::next(It)->Tokens.begin()) &&
                 "A semicolon already consumed by the next node?");

          S->addChild(
              C, construct<syntax::Leaf>(llvm::makeArrayRef(TrailingSemi, 1)));
          It->Tokens =
              llvm::makeArrayRef(It->Tokens.begin(), std::next(TrailingSemi));
        }
      });
}

void TreeBuilder::learnNodeImpl(
    SourceLocation Begin, SourceLocation End, syntax::TreeNode *New,
    llvm::function_ref<void(std::vector<RangedNode>::iterator FirstChild)>
        TweakChildren) {
  assert(Begin.isValid());
  assert(End.isValid());
  assert(Begin == End ||
         Corpus.sourceManager().isBeforeInTranslationUnit(Begin, End));

  auto *BeginTok = findToken(Begin);
  auto *EndTok = findToken(End);
  assert(BeginTok <= EndTok);
  ++EndTok;

  // FIXME: use binary search.
  auto FirstChild =
      std::find_if(NodesInProgress.rbegin(), NodesInProgress.rend(),
                   [&](const RangedNode &L) {
                     if (&L.Tokens.front() < BeginTok) {
                       assert(&L.Tokens.back() <= EndTok);
                       return true;
                     }
                     return false;
                   })
          .base();

  if (TweakChildren)
    TweakChildren(FirstChild);

  auto *NextTok = BeginTok;
  auto CoverTokens = [&](const syntax::Token *UpTo) {
    if (NextTok == UpTo)
      return;
    assert(NextTok < UpTo);
    New->addChild(corpus(),
                  construct<syntax::Leaf>(llvm::makeArrayRef(NextTok, UpTo)));
    NextTok = UpTo;
  };
  for (auto It = FirstChild; It != NodesInProgress.end(); ++It) {
    // Add non-coverred ranges as token nodes.
    CoverTokens(&It->Tokens.front());

    New->addChild(corpus(), It->Node);
    NextTok = It->Tokens.end();
  }
  CoverTokens(EndTok);

  NodesInProgress.erase(FirstChild, NodesInProgress.end());
  NodesInProgress.push_back(
      RangedNode{llvm::makeArrayRef(BeginTok, EndTok), New});
}

void TreeBuilder::learnRoot() {
  auto Tokens = Corpus.mainFile().tokens();
  learnNode(Tokens.front().location(), Tokens.back().location(),
            construct<syntax::TranslationUnit>());

  assert(NodesInProgress.size() == 1);
  assert(NodesInProgress.front().Node->kind() ==
         syntax::NodeKind::TranslationUnit);

  Root = cast<syntax::TranslationUnit>(NodesInProgress.front().Node);
  NodesInProgress.clear();
}

const syntax::Token *TreeBuilder::findToken(SourceLocation TokLoc) const {
  auto Tokens = Corpus.mainFile().tokens();
  auto &SM = Corpus.sourceManager();
  auto It = std::lower_bound(Tokens.begin(), Tokens.end(), TokLoc,
                             [&](const syntax::Token &L, SourceLocation R) {
                               return SM.isBeforeInTranslationUnit(
                                   L.location(), R);
                             });
  assert(It != Tokens.end());
  assert(SM.getFileOffset(It->location()) == SM.getFileOffset(TokLoc));
  return &*It;
}

template <class TNode, class... Args>
TNode *TreeBuilder::construct(Args &&... args) {
  auto *Res = Corpus.construct<TNode>(std::forward<Args>(args)...);
  if (auto *L = llvm::dyn_cast<syntax::Leaf>(Res))
    L->markOriginal();
  return Res;
}

} // namespace

syntax::TranslationUnit *
syntax::buildSyntaxTree(Corpus &C, const TranslationUnitDecl &TU) {
  TreeBuilder Builder(C);
  BuildTreeVisitor(TU.getASTContext(), Builder).TraverseAST(TU.getASTContext());
  return std::move(Builder).root();
}
