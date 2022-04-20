module AST where

-- Jack terminals
data Keyword = Keyword String deriving (Show, Eq)
data Symbol = Symbol String deriving (Show, Eq)
data Identifier = Identifier String deriving (Show, Eq)
data Op = Op String deriving (Show, Eq)
type VarName = Identifier

-- expression: term (op term)*
data Expr = Expr Term [(Op, Term)] deriving (Show, Eq)

-- subroutineCall: subroutineName '(' expressionList ')' |
--               (className | varName) '.' subroutineName '(' expressionList ')'
data SubCall = SubCallName Identifier Symbol [Expr] Symbol
  | SubCallClassOrVar Identifier Symbol Identifier Symbol [Expr] Symbol
  deriving (Show, Eq)

data Term = IntegerConstant Int
  | StringConstant String
  | KeywordConstant String
  | VarName VarName
  | UnaryOp String Term
  | VarNameExpr VarName Symbol Expr Symbol
  | ParenExpr Symbol Expr Symbol
  | SubroutineCall SubCall
  deriving (Show, Eq)

data LetVarName = LetVarName VarName
  | LetVarNameExpr VarName Symbol Expr Symbol
  deriving (Show, Eq)

data Statement =
  -- letStatement: 'let' varName ('[' expr ']')? '=' expr ';'
  Let Symbol LetVarName Symbol Expr Symbol
  deriving (Show, Eq)
