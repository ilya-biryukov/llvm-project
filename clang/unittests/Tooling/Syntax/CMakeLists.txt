set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Support
  )

add_clang_unittest(TokenBufferTest
  TokenBufferTest.cpp
)

target_link_libraries(TokenBufferTest
  PRIVATE
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangSerialization
  clangTooling
  clangToolingSyntax
  LLVMTestingSupport
  )
