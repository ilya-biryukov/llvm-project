#include "clang/Tooling/Syntax/Nodes.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Syntax/Corpus.h"
#include "clang/Tooling/Syntax/Syntax.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

llvm::StringRef syntax::toString(NodeKind K) {
  switch (K) {
  case NodeKind::Recovery:
    return "recovery";
  case NodeKind::UnkownExpr:
    return "unknown-expr";
  case NodeKind::PredefinedExpr:
    return "predefined-expr";
  case NodeKind::ReferenceExpr:
    return "reference-expr";
  case NodeKind::ParenExpr:
    return "paren-expr";
  case NodeKind::UnaryExpr:
    return "unary-expr";
  case NodeKind::BinaryExpr:
    return "binary-expr";
  case NodeKind::LiteralExpr:
    return "literal-expr";
  case NodeKind::UnknownStmt:
    return "unknown-stmt";
  case NodeKind::ExpressionStmt:
    return "expression-stmt";
  case NodeKind::CompoundStmt:
    return "compound-stmt";
  case NodeKind::IfStmt:
    return "if-stmt";
  case NodeKind::ReturnStmt:
    return "return-stmt";
  case NodeKind::WhileStmt:
    return "while-stmt";
  case NodeKind::DoStmt:
    return "do-stmt";
  case NodeKind::ForStmt:
    return "for-stmt";
  case NodeKind::RangeBasedForStmt:
    return "range-based-for-stmt";
  case NodeKind::TranslationUnit:
    return "translation-unit";
  case NodeKind::Token:
    return "tokens";
  }
  llvm_unreachable("invalid NodeKind");
  return "<invalid kind>";
}

namespace {
syntax::Node *shallowCopy(const syntax::Node *N, syntax::Corpus &C) {
  using syntax::NodeKind;

  switch (N->kind()) {
  case NodeKind::UnkownExpr:
    return C.construct<syntax::UnknownExpr>();
  case NodeKind::LiteralExpr:
    return C.construct<syntax::LiteralExpr>();
  case NodeKind::PredefinedExpr:
    return C.construct<syntax::PredefinedExpr>();
  case NodeKind::ReferenceExpr:
    return C.construct<syntax::ReferenceExpr>();
  case NodeKind::UnaryExpr:
    return C.construct<syntax::UnaryExpr>();
  case NodeKind::BinaryExpr:
    return C.construct<syntax::BinaryExpr>();
  case NodeKind::ParenExpr:
    return C.construct<syntax::ParenExpr>();
  case NodeKind::UnknownStmt:
    return C.construct<syntax::UnknownStmt>();
  case NodeKind::ExpressionStmt:
    return C.construct<syntax::ExpressionStmt>();
  case NodeKind::CompoundStmt:
    return C.construct<syntax::CompoundStmt>();
  case NodeKind::IfStmt:
    return C.construct<syntax::IfStmt>();
  case NodeKind::ReturnStmt:
    return C.construct<syntax::ReturnStmt>();
  case NodeKind::WhileStmt:
    return C.construct<syntax::WhileStmt>();
  case NodeKind::DoStmt:
    return C.construct<syntax::DoStmt>();
  case NodeKind::ForStmt:
    return C.construct<syntax::ForStmt>();
  case NodeKind::RangeBasedForStmt:
    return C.construct<syntax::RangeBasedForStmt>();
  case NodeKind::TranslationUnit:
    return C.construct<syntax::TranslationUnit>();
  case NodeKind::Recovery:
    return C.construct<syntax::RecoveryNode>();
  case NodeKind::Token:
    return C.construct<syntax::Leaf>(llvm::cast<syntax::Leaf>(N)->tokens());
  }
  llvm_unreachable("invalid node kind");
}

void dumpTokens(llvm::raw_ostream &OS, ArrayRef<syntax::Token> Tokens,
                const SourceManager &SM, const LangOptions &LO) {
  for (const auto &T : Tokens)
    OS << T.text(SM) << " ";
}

void dumpTree(llvm::raw_ostream &OS, const syntax::Node *N,
              const syntax::Corpus &C, std::vector<bool> IndentMask) {
  if (auto *L = llvm::dyn_cast<syntax::Leaf>(N)) {
    dumpTokens(OS, L->tokens(), C.sourceManager(), C.langOptions());
    OS << "\n";
    return;
  }

  auto *T = llvm::cast<syntax::TreeNode>(N);
  OS << toString(T->kind()) << "\n";

  for (auto It = T->children().begin(); It != T->children().end(); ++It) {
    for (bool Filled : IndentMask) {
      if (Filled)
        OS << "| ";
      else
        OS << "  ";
    }
    if (std::next(It) == T->children().end()) {
      OS << "`-";
      IndentMask.push_back(false);
    } else {
      OS << "|-";
      IndentMask.push_back(true);
    }
    dumpTree(OS, *It, C, IndentMask);
    IndentMask.pop_back();
  }
}

syntax::Node *deepCopy(const syntax::Node *N, syntax::Corpus &C) {
  auto *New = shallowCopy(N, C);
  auto *T = llvm::dyn_cast<syntax::TreeNode>(N);
  if (!T)
    return New;
  for (auto *Child : T->children())
    cast<syntax::TreeNode>(New)->addChild(C, deepCopy(Child, C));
  return New;
}
} // namespace

std::string syntax::Node::dump(const Corpus &C) const {
  std::string Str;
  llvm::raw_string_ostream OS(Str);
  dump(C, OS);
  return std::move(OS.str());
}

void syntax::Node::dump(const Corpus &C, llvm::raw_ostream &OS) const {
  dumpTree(OS, this, C, /*IndentMask=*/{});
}

std::string syntax::Node::dumpTokens(const Corpus &C) const {
  std::string Str;
  llvm::raw_string_ostream OS(Str);
  dumpTokens(C, OS);
  return std::move(OS.str());
}

void syntax::Node::dumpTokens(const Corpus &C, llvm::raw_ostream &OS) const {
  traverse(this, [&](const syntax::Node *N) {
    auto *L = llvm::dyn_cast<syntax::Leaf>(N);
    if (!L)
      return;
    ::dumpTokens(OS, L->tokens(), C.sourceManager(), C.langOptions());
  });
}

bool syntax::Node::isModifiable(const Corpus &C) const {
  // FIXME: this should be computed on construction and stored as a flag.
  const Token *Begin = nullptr;
  const Token *End = nullptr;

  auto &SM = C.sourceManager();
  bool CanCopy = true;
  traverse(this, [&](const Node *N) {
    auto *L = llvm::dyn_cast<Leaf>(N);
    if (!L)
      return;
    // Just extend the current range.
    if (L->tokens().data() == End) {
      End = L->tokens().data() + L->tokens().size();
      return;
    }
    if (Begin) {
      // Check we don't cross macro expansions.
      CanCopy &= C.findBuffer(Begin->location())
                     ->toOffsetRange(Begin, End, SM)
                     .hasValue();
    }
    // Start a new range.
    Begin = L->tokens().data();
    End = L->tokens().data() + L->tokens().size();
  });
  if (Begin && CanCopy) {
    // Check we don't cross macro expansions.
    CanCopy &= C.findBuffer(Begin->location())
                   ->toOffsetRange(Begin, End, SM)
                   .hasValue();
  }
  return CanCopy;
}

syntax::Node *syntax::Node::copy(Corpus &C) const {
  assert(isCopyable(C));
  return deepCopy(this, C);
}

void syntax::TreeNode::addChild(Corpus &C, Node *Child) {
  assert(Child->Parent == nullptr);

  Child->Parent = this;
  Children.push_back(C.allocator(), Child);
}

syntax::Node *syntax::TreeNode::replaceChild(Corpus &C, Node *Child,
                                             const Node *Replacement) {
  assert(Child->isModifiable(C));
  // Copy the replacement node to avoid accidentally breaking its parent.
  auto *New = Replacement->copy(C);
  New->Parent = this;
  // Replace the child.
  auto It = llvm::find(Children, Child);
  assert(It != Children.end());
  *It = New;
  Child->Parent = nullptr;
  return New;
}

syntax::Node **
syntax::TreeNode::findChild(llvm::function_ref<bool(Node *)> Predicate,
                            int Skip) {
  int Skipped = 0;
  for (auto It = Children.begin(); It != Children.end(); ++It) {
    if (!Predicate(*It))
      continue;
    if (Skipped == Skip)
      return It;
    ++Skipped;
  }
  llvm_unreachable("Child not found. Broken syntax tree?");
  return Children.end();
}

namespace {
static bool isExpr(syntax::Node *N) { return isa<syntax::Expr>(N); }
} // namespace

syntax::Expr *syntax::UnaryExpr::arg() {
  return cast<Expr>(*findChild(isExpr, 0));
}
syntax::Expr *syntax::BinaryExpr::lhs() {
  return cast<Expr>(*findChild(isExpr, 0));
}
syntax::TokenPtr syntax::BinaryExpr::operatorTok() {
  auto LHSIt = findChild(isExpr, 0);
  ++LHSIt;
  assert(LHSIt != children().end());
  // FIXME: the operator token might be non-first.
  TokenPtr P;
  P.Leaf = cast<Leaf>(*LHSIt);
  P.Token = P.Leaf->tokens().data();
  return P;
}

tok::TokenKind syntax::BinaryExpr::operatorKind() {
  return operatorTok().Token->kind();
}

syntax::Expr *syntax::BinaryExpr::rhs() {
  return cast<Expr>(*findChild(isExpr, 1));
}

syntax::Expr *syntax::ParenExpr::inner() {
  return cast<Expr>(*findChild(isExpr));
}
