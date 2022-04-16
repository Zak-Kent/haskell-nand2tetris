module AST where

-- Jack terminals
data Keyword = Keyword String deriving (Show)
data Symbol = Symbol String deriving (Show)
data Identifier = Identifier String deriving (Show)
data Op = Op String deriving (Show)

-- expression: term (op term)*
data Expr = Expr Term [(Op, Term)] deriving (Show)

-- subroutineCall: subroutineName '(' expressionList ')' |
--               (className | varName) '.' subroutineName '(' expressionList ')'
data SubCall = SubCallName Identifier Symbol [Expr] Symbol
  | SubCallClassOrVar Identifier Symbol Identifier Symbol [Expr] Symbol
  deriving (Show)

data Term = IntegerConstant Int
  | StringConstant String
  | KeywordConstant String
  | VarName Identifier
  | UnaryOp String Term
  | VarNameExpr Identifier Symbol Expr Symbol
  | ParenExpr Symbol Expr Symbol
  | SubroutineCall SubCall
  deriving (Show)
