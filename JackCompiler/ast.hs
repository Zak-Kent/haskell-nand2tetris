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

data Else = Else Symbol Symbol Statements Symbol deriving (Show, Eq)

type Statements = [Statement]
data Statement =
  -- 'let' varName ('[' expr ']')? '=' expr ';'
  Let Symbol LetVarName Symbol Expr Symbol
  -- 'if' '(' expr ')' '{' statements '}' ('else' '{' statements '}')?
  | If Symbol Symbol Expr Symbol Symbol Statements Symbol (Maybe Else)
  -- 'while' '(' expr ')' '{' statements '}'
  | While Symbol Symbol Expr Symbol Symbol Statements Symbol
  | Do Symbol SubCall
  -- 'return' (expr)? ';'
  | Return Symbol (Maybe Expr) Symbol
  deriving (Show, Eq)
