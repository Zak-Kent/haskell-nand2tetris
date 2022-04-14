module AST where

-- Jack terminals
data Keyword = Keyword String deriving (Show)
data Symbol = Symbol String deriving (Show)
data IntegerConstant = IntegerConstant Int deriving (Show)
data StringConstant = StringConstant String deriving (Show)
data Identifier = Identifier String deriving (Show)

-- Jack expressions
data KeywordConstant = KeywordConstant String deriving (Show)
data UnaryOp = UnaryOp String deriving (Show)
data Op = Op String deriving (Show)