# hscc

Compiler frontend written in Haskell with following modules:
* Lexer:
  - Regex: Parsing, AST, DFA Generation
  - DFA: Calculation, Reduction, Minimize
* Parser:
  - CFG: Calculation (first, follow, select)
  - LL, LR: WIP
* C Frontend:
  - Syntax: Lexing & parsing with parsec
  - Semantic: AST analysis, type computing & validation, compile-time evaluating
  - Codegen: LLVM
