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

data Else = Else Keyword Symbol Statements Symbol deriving (Show, Eq)

type Statements = [Statement]
data Statement =
  -- 'let' varName ('[' expr ']')? '=' expr ';'
  Let Keyword LetVarName Symbol Expr Symbol
  -- 'if' '(' expr ')' '{' statements '}' ('else' '{' statements '}')?
  | If Keyword Symbol Expr Symbol Symbol Statements Symbol (Maybe Else)
  -- 'while' '(' expr ')' '{' statements '}'
  | While Keyword Symbol Expr Symbol Symbol Statements Symbol
  | Do Keyword SubCall
  -- 'return' (expr)? ';'
  | Return Keyword (Maybe Expr) Symbol
  deriving (Show, Eq)

data Type = TKeyword Keyword
  | TIdentifier Identifier
  deriving (Show, Eq)

-- 'var' type varName (',' varName)* ';'
data VarDec = VarDec Keyword Type VarName [VarName] Symbol
  deriving (Show, Eq)

-- '{' varDec* statements '}'
data SubroutineBody = SubroutineBody Symbol [VarDec] Statements Symbol
  deriving (Show, Eq)

-- ((type varName) (',' type varName)*)?
data ParameterList = ParameterList [(Type, VarName)]
  deriving (Show, Eq)

-- ('constructor' | 'function' |'method') ('void' | type) subroutineName
--   '(' parameterLIst ')' subroutineBody
data SubroutineDec = SubroutineDec Keyword Type Identifier
                       Symbol ParameterList Symbol SubroutineBody
                       deriving (Show, Eq)

-- ('static' 'field') type varName (',' varName)* ';'
data ClassVarDec = ClassVarDec Keyword Type VarName [VarName] Symbol
  deriving (Show, Eq)

-- 'class' className '{' classVarDec* subroutineDec* '}'
data Class = Class Keyword Identifier Symbol [ClassVarDec] [SubroutineDec] Symbol
  deriving (Show, Eq)
