set(LLVM_LINK_COMPONENTS Support)

add_clang_library(clangToolingSyntax
  BuildFromAST.cpp
  ComputeReplacements.cpp
  Corpus.cpp
  Nodes.cpp
  Syntax.cpp
  TokenBuffer.cpp
  Transform.cpp

  LINK_LIBS
  clangBasic
  clangFrontend
  clangLex
  clangAST
  clangToolingCore
  )
