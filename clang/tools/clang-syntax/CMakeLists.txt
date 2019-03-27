set(LLVM_LINK_COMPONENTS Support)

add_clang_tool(clang-syntax
  SyntaxMain.cpp
  )

target_link_libraries(clang-syntax
  PRIVATE
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangTooling
  clangToolingCore
  clangToolingSyntax
)
