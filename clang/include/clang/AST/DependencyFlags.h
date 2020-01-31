//===--- DependencyFlags.h ------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_CLANG_AST_DEPENDENCYFLAGS_H
#define LLVM_CLANG_AST_DEPENDENCYFLAGS_H

#include "clang/Basic/BitmaskEnum.h"
#include "llvm/ADT/BitmaskEnum.h"
#include <cstdint>

namespace clang {
struct ExprDependenceScope {
  enum ExprDependence : uint8_t {
    UnexpandedPack = 1,
    Instantiation = 2,
    Error = 4,
    Type = 8,
    Value = 16,

    None = 0,
    All = 31,

    TypeValue = Type | Value,
    TypeInstantiation = Type | Instantiation,
    ValueInstantiation = Value | Instantiation,
    TypeValueInstantiation = Type | Value | Instantiation,

    LLVM_MARK_AS_BITMASK_ENUM(/*LargestValue=*/Value)
  };
};
using ExprDependence = ExprDependenceScope::ExprDependence;
static constexpr unsigned ExprDependenceBits = 5;

#define LLVM_COMMON_DEPENDENCE(NAME)                                           \
  struct NAME##Scope {                                                         \
    enum NAME {                                                                \
      UnexpandedPack = 1,                                                      \
      Instantiation = 2,                                                       \
      Error = 4,                                                              \
      Dependent = 8,                                                           \
                                                                               \
      None = 0,                                                                \
      DependentInstantiation = Dependent | Instantiation,                      \
      All = 15,                                                                \
                                                                               \
      LLVM_MARK_AS_BITMASK_ENUM(/*LargestValue=*/Dependent)                    \
    };                                                                         \
  };                                                                           \
  using NAME = NAME##Scope::NAME;                                              \
  static constexpr unsigned NAME##Bits = 4;

LLVM_COMMON_DEPENDENCE(TypeDependence)
LLVM_COMMON_DEPENDENCE(NestedNameSpecifierDependence)
LLVM_COMMON_DEPENDENCE(TemplateNameDependence)
LLVM_COMMON_DEPENDENCE(TemplateArgumentDependence)
#undef LLVM_COMMON_DEPENDENCE

/// Computes dependencies of a reference with the name having template arguments
/// with \p TA dependencies.
inline ExprDependence toExprDependence(TemplateArgumentDependence TA) {
  auto E =
      static_cast<ExprDependence>(TA & ~TemplateArgumentDependence::Dependent);
  if (TA & TemplateArgumentDependence::Dependent)
    return E | ExprDependence::Type | ExprDependence::Value;
  return E;
}
inline ExprDependence toExprDependence(TypeDependence TD) {
  // This hack works because TypeDependence and TemplateArgumentDependence
  // share the same bit representation.
  return toExprDependence(static_cast<TemplateArgumentDependence>(TD));
}
inline ExprDependence toExprDependence(NestedNameSpecifierDependence NSD) {
  // This hack works because TypeDependence and TemplateArgumentDependence
  // share the same bit representation.
  return toExprDependence(static_cast<TemplateArgumentDependence>(NSD)) &
         ~ExprDependence::TypeValue;
}
inline ExprDependence turnTypeToValueDependence(ExprDependence D) {
  // Type-dependent expressions are always be value-dependent, so we simply drop
  // type dependency.
  return D & ~ExprDependence::Type;
}

inline NestedNameSpecifierDependence
toNestedNameSpecifierDependendence(TypeDependence D) {
  // This works because both classes share the same bit representation.
  return static_cast<NestedNameSpecifierDependence>(D);
}

inline TemplateArgumentDependence
toTemplateArgumentDependence(TypeDependence D) {
  // This works because both classes share the same bit representation.
  return static_cast<TemplateArgumentDependence>(D);
}
inline TemplateArgumentDependence
toTemplateArgumentDependence(TemplateNameDependence D) {
  // This works because both classes share the same bit representation.
  return static_cast<TemplateArgumentDependence>(D);
}
inline TemplateArgumentDependence
toTemplateArgumentDependence(ExprDependence ED) {
  TemplateArgumentDependence TAD = static_cast<TemplateArgumentDependence>(
      ED & ~(ExprDependence::Type | ExprDependence::Value));
  if (ED & (ExprDependence::Type | ExprDependence::Value))
    TAD |= TemplateArgumentDependence::Dependent;
  return TAD;
}

inline TemplateNameDependence
toTemplateNameDependence(NestedNameSpecifierDependence D) {
  return static_cast<TemplateNameDependence>(D);
}

LLVM_ENABLE_BITMASK_ENUMS_IN_NAMESPACE();

} // namespace clang
#endif
