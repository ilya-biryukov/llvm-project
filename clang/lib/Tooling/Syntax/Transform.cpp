#include "clang/Tooling/Syntax/Transform.h"
#include "clang/Basic/OperatorPrecedence.h"
#include "clang/Tooling/Syntax/Syntax.h"
#include "llvm/Support/Casting.h"

using namespace clang;

syntax::Leaf *syntax::createPunctuator(Corpus &C, tok::TokenKind Kind) {
  const char *Spelling = tok::getPunctuatorSpelling(Kind);
  assert(Spelling);
  // FIXME: share one buffer with all punctuator tokens.
  const TokenBuffer &TB =
      C.tokenizeBuffer(llvm::MemoryBuffer::getMemBuffer(Spelling)).second;
  assert(TB.tokens().front().kind() == Kind);
  return C.construct<Leaf>(TB.tokens().take_front(1));
}

syntax::ParenExpr *syntax::createParenExpr(Corpus &C, syntax::Expr *Inner) {
  auto *R = C.construct<ParenExpr>();
  R->addChild(C, createPunctuator(C, tok::l_paren));
  R->addChild(C, Inner->copy(C));
  R->addChild(C, createPunctuator(C, tok::r_paren));
  return R;
}

syntax::BinaryExpr *syntax::createBinaryExpr(Corpus &C, Expr *LHS,
                                             tok::TokenKind Operator,
                                             Expr *RHS) {
  auto *R = C.construct<BinaryExpr>();
  R->addChild(C, C.construct<UnknownExpr>());
  R->addChild(C, createPunctuator(C, Operator));
  R->addChild(C, C.construct<UnknownExpr>());

  // FIXME: the layering is unfortunate, find other ways to reuse parenthesizing
  //        code.
  (void)replaceExpression(C, R->lhs(), LHS);
  (void)replaceExpression(C, R->rhs(), RHS);
  return R;
}

namespace {
bool needsParentheses(syntax::Expr *Old, syntax::Expr *New) {
  // FIXME: handle other cases: different associativity, ternary operator, etc.
  auto *NewBinary = llvm::dyn_cast<syntax::BinaryExpr>(New);
  if (!NewBinary)
    return false;
  prec::Level NewPrec =
      getBinOpPrecedence(NewBinary->operatorTok().Token->kind(),
                         /*GreaterThanIsOperator=*/true,
                         /*CPlusPlus11=*/true); // FIXME: 'CPlusPlus11' should
                                                // be taken from LangOpts.
  auto *Parent = llvm::dyn_cast<syntax::BinaryExpr>(Old->parent());
  if (!Parent)
    return false;
  prec::Level Prec = getBinOpPrecedence(Parent->operatorKind(),
                                        /*GreaterThanIsOperator=*/true,
                                        /*CPlusPlus11=*/true);
  return NewPrec < Prec;
}
} // namespace

syntax::Expr *syntax::replaceExpression(syntax::Corpus &C, syntax::Expr *Old,
                                        syntax::Expr *New) {
  assert(Old->isModifiable(C));
  assert(New->isCopyable(C));

  if (needsParentheses(Old, New))
    New = createParenExpr(C, New);
  return cast<Expr>(Old->parent()->replaceChild(C, Old, New));
}
