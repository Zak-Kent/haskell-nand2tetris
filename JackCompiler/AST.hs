module AST where

-- Jack terminals
data Keyword = Keyword String deriving (Show, Eq)
data Symbol = Symbol String deriving (Show, Eq)
data Identifier = Identifier String deriving (Show, Eq)
type VarName = Identifier

data Tree a = Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = (Leaf (f a))
mapTree f (Node lb op rb) = (Node (mapTree f lb) (f op) (mapTree f rb))

instance Functor Tree where
  fmap = mapTree

instance Foldable Tree where
  foldr f z (Leaf a) = f a z
  foldr f z (Node lb op rb) = (foldr f (foldr f (f op z) lb) rb)

-- expression: term (op term)*
-- expr tree ex: 5 + 6 + 7
{-
      +
     / \
    5   +
       / \
      6   7
-}
data Expr = Expr (Tree Term)
  deriving (Show, Eq)

-- subroutineCall: subroutineName '(' expressionList ')' |
--               (className | varName) '.' subroutineName '(' expressionList ')'
data SubCall = SubCallName Identifier [Expr]
  | SubCallClassOrVar Identifier Identifier [Expr]
  deriving (Show, Eq)

data Term = IntegerConstant Int
  | StringConstant String
  | KeywordConstant String
  | VarName VarName
  | UnaryOp Symbol Term
  | VarNameExpr VarName Expr
  | ParenExpr Expr
  | SubroutineCall SubCall
  | Op Symbol
  deriving (Show, Eq)

data LetVarName = LetVarName VarName
  | LetVarNameExpr VarName Expr
  deriving (Show, Eq)

data Else = Else Statements deriving (Show, Eq)

type Statements = [Statement]
data Statement =
  -- 'let' varName ('[' expr ']')? '=' expr ';'
  Let LetVarName Expr
  -- 'if' '(' expr ')' '{' statements '}' ('else' '{' statements '}')?
  | If Expr Statements (Maybe Else)
  -- 'while' '(' expr ')' '{' statements '}'
  | While Expr Statements
  | Do SubCall
  -- 'return' (expr)? ';'
  | Return (Maybe Expr)
  deriving (Show, Eq)

data Type = TKeyword Keyword
  | TIdentifier Identifier
  deriving (Show, Eq)

-- 'var' type varName (',' varName)* ';'
data VarDec = VarDec Type VarName [VarName]
  deriving (Show, Eq)

-- '{' varDec* statements '}'
data SubroutineBody = SubroutineBody [VarDec] Statements
  deriving (Show, Eq)

-- ((type varName) (',' type varName)*)?
data ParameterList = ParameterList [(Type, VarName)]
  deriving (Show, Eq)

-- ('constructor' | 'function' |'method') ('void' | type) subroutineName
--   '(' parameterLIst ')' subroutineBody
data SubroutineDec = SubroutineDec Keyword Type Identifier
                     ParameterList SubroutineBody
                       deriving (Show, Eq)

-- ('static' 'field') type varName (',' varName)* ';'
data ClassVarDec = ClassVarDec Keyword Type VarName [VarName]
  deriving (Show, Eq)

-- 'class' className '{' classVarDec* subroutineDec* '}'
data Class = Class Keyword Identifier [ClassVarDec] [SubroutineDec]
  deriving (Show, Eq)
