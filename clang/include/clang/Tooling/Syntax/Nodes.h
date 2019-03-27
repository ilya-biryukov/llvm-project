//===- Nodes.h - syntax nodes for C++ grammar constructs ------*- C++ -*-=====//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_CLANG_TOOLING_SYNTAX_NODES_H
#define LLVM_CLANG_TOOLING_SYNTAX_NODES_H
#include "clang/Lex/Token.h"
#include "clang/Tooling/Syntax/NodeList.h"
#include "clang/Tooling/Syntax/TokenBuffer.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
namespace clang {
namespace syntax {

class Corpus;

enum class NodeKind {
  Recovery,
  /// Declarations.
  TranslationUnit,
  /// Expressions.
  UnkownExpr,
  LiteralExpr,
  PredefinedExpr,
  ReferenceExpr,
  UnaryExpr,
  BinaryExpr,
  ParenExpr,
  /// Statements.
  UnknownStmt,
  ExpressionStmt,
  CompoundStmt,
  IfStmt,
  ReturnStmt,
  WhileStmt,
  DoStmt,
  ForStmt,
  RangeBasedForStmt,
  /// A leaf token node.
  Token,
  /// Helper constants for implementing casting.
  FirstExpr = UnkownExpr,
  LastExpr = ParenExpr,
  FirstStmt = UnknownStmt,
  LastStmt = RangeBasedForStmt,
};

/// For debugging purposes.
llvm::StringRef toString(NodeKind K);

class TreeNode;

/// A node in a syntax tree.
class Node {
public:
  Node(NodeKind Kind) : Kind(Kind) {}
  NodeKind kind() const { return Kind; }

  /// Construct a deep copy of the node. Useful in case the current node will be
  /// mutated later.
  /// EXPECTS: isCopyable() == true.
  Node *copy(Corpus &C) const;
  /// Checks if the node can be copied. See isModifiable() for details,
  /// preconditions for copy are the same.
  bool isCopyable(const Corpus &C) const { return isModifiable(C); }
  /// Checks whether this node can be modified. We disallow mutations of the
  /// subtrees that cannot be replaced without expanding macros. In particular,
  /// we only allow to modify full macro calls or macro arguments.
  bool isModifiable(const Corpus &C) const;

  const TreeNode *parent() const { return Parent; }
  TreeNode *parent() { return Parent; }

  std::string dump(const Corpus &C) const;
  void dump(const Corpus &C, llvm::raw_ostream &OS) const;

  std::string dumpTokens(const Corpus &C) const;
  void dumpTokens(const Corpus &C, llvm::raw_ostream &OS) const;

private:
  // TreeNode is allowed to change the Parent link.
  friend class TreeNode;
  TreeNode *Parent = nullptr;
  NodeKind Kind;
};

/// A leaf node, pointing to a consecutive range of tokens in some token buffer.
class Leaf final : public Node {
public:
  Leaf(llvm::ArrayRef<syntax::Token> Tokens)
      : Node(NodeKind::Token), Tokens(Tokens) {}

  static bool classof(const Node *N) { return N->kind() == NodeKind::Token; }

  llvm::ArrayRef<syntax::Token> tokens() const { return Tokens; }
  bool isOriginal() const { return IsOriginal; }

  void markOriginal() { IsOriginal = true; }

private:
  /// Whether this node was part of the original document tree. This is used by
  /// the algorithm computing replacements.
  /// FIXME: this flag should not take extra storage, pack it into Kind.
  bool IsOriginal = false;
  llvm::ArrayRef<Token> Tokens;
};

/// A pointer into a particular token inside a leaf node.
struct TokenPtr {
  const Leaf *Leaf = nullptr;
  const Token *Token = nullptr;
};

/// A composite tree node that has children.
class TreeNode : public Node {
public:
  using children_range = llvm::iterator_range<Node **>;
  using children_const_range = llvm::iterator_range<const Node *const *>;

  using Node::Node;
  static bool classof(const Node *N) { return N->kind() < NodeKind::Token; }

  children_range children() {
    return llvm::make_range(Children.begin(), Children.end());
  }
  children_const_range children() const {
    return llvm::make_range(Children.begin(), Children.end());
  }

  /// EXPECTS: Child->isModifiable() && Replacement->isCopyable()
  Node *replaceChild(Corpus &C, Node *Child, const Node *Replacement);
  void addChild(Corpus &C, Node *Child);

protected:
  /// Find the child that matches \p Predicate, skipping first \p Skip matches.
  /// This is not allowed to fail, the callers are expected to know the
  /// structure of the AST.
  Node **findChild(llvm::function_ref<bool(Node *)> Predicate, int Skip = 0);

private:
  NodeList Children;
};

/// A tree node of an unknown kind, i.e. a syntax error or an unimplemented
/// construct.
class RecoveryNode final : public TreeNode {
public:
  RecoveryNode() : TreeNode(NodeKind::Recovery) {}

  static bool classof(const Node *N) { return N->kind() == NodeKind::Recovery; }
};

/// A root node for a translation unit. Parent is always null.
class TranslationUnit : public TreeNode {
public:
  TranslationUnit() : TreeNode(NodeKind::TranslationUnit) {}

  static bool classof(const Node *N) {
    return N->kind() == NodeKind::TranslationUnit;
  }
};

/// A base class for all expression nodes.
class Expr : public TreeNode {
public:
  using TreeNode::TreeNode;
  static bool classof(const Node *N) {
    return NodeKind::FirstExpr <= N->kind() && N->kind() <= NodeKind::LastExpr;
  }
};

/// An expression in the clang AST which is missing an equivalent in the syntax
/// tree.
/// This will be removed when we handle all of the AST nodes.
class UnknownExpr final : public Expr {
public:
  UnknownExpr() : Expr(NodeKind::UnkownExpr) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::UnkownExpr;
  }
};

/// All literal expressions, including integral literals, character literals,
/// etc.
/// FIXME: this should be a base class and the literals should be subclasses.
class LiteralExpr final : public Expr {
public:
  LiteralExpr() : Expr(NodeKind::LiteralExpr) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::LiteralExpr;
  }
};

class PredefinedExpr final : public Expr {
public:
  PredefinedExpr() : Expr(NodeKind::PredefinedExpr) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::PredefinedExpr;
  }
};

/// '+a', '!foo', etc.
class UnaryExpr final : public Expr {
public:
  UnaryExpr() : Expr(NodeKind::UnaryExpr) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::UnaryExpr;
  }

  Expr *arg();
};

/// a = b, a != c, etc.
class BinaryExpr final : public Expr {
public:
  BinaryExpr() : Expr(NodeKind::BinaryExpr) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::BinaryExpr;
  }

  Expr *lhs();
  TokenPtr operatorTok();
  tok::TokenKind operatorKind();
  Expr *rhs();
};

/// A possibly qualified reference expression.
class ReferenceExpr final : public Expr {
public:
  ReferenceExpr() : Expr(NodeKind::ReferenceExpr) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::ReferenceExpr;
  }
};

/// A parenthesized expression.
class ParenExpr final : public Expr {
public:
  ParenExpr() : Expr(NodeKind::ParenExpr) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::ParenExpr;
  }

  Expr *inner();
};

/// A base class for all statements.
class Stmt : public TreeNode {
public:
  using TreeNode::TreeNode;
  static bool classof(const Node *N) {
    return NodeKind::FirstStmt <= N->kind() && N->kind() <= NodeKind::LastStmt;
  }
};

class UnknownStmt final : public Stmt {
public:
  UnknownStmt() : Stmt(NodeKind::UnknownStmt) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::UnknownStmt;
  }
};

class ExpressionStmt final : public Stmt {
public:
  ExpressionStmt() : Stmt(NodeKind::ExpressionStmt) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::ExpressionStmt;
  }
};

class CompoundStmt final : public Stmt {
public:
  CompoundStmt() : Stmt(NodeKind::CompoundStmt) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::CompoundStmt;
  }
  NodeKind kind() const { return NodeKind::CompoundStmt; }
};

class IfStmt final : public Stmt {
public:
  IfStmt() : Stmt(NodeKind::IfStmt) {}
  static bool classof(const Node *N) { return N->kind() == NodeKind::IfStmt; }
};

class ReturnStmt final : public Stmt {
public:
  ReturnStmt() : Stmt(NodeKind::ReturnStmt) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::ReturnStmt;
  }
};

class WhileStmt final : public Stmt {
public:
  WhileStmt() : Stmt(NodeKind::WhileStmt) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::WhileStmt;
  }
};

class DoStmt final : public Stmt {
public:
  DoStmt() : Stmt(NodeKind::DoStmt) {}
  static bool classof(const Node *N) { return N->kind() == NodeKind::DoStmt; }
};

class ForStmt final : public Stmt {
public:
  ForStmt() : Stmt(NodeKind::ForStmt) {}
  static bool classof(const Node *N) { return N->kind() == NodeKind::ForStmt; }
};

class RangeBasedForStmt final : public Stmt {
public:
  RangeBasedForStmt() : Stmt(NodeKind::RangeBasedForStmt) {}
  static bool classof(const Node *N) {
    return N->kind() == NodeKind::RangeBasedForStmt;
  }
};

} // namespace syntax
} // namespace clang
#endif
