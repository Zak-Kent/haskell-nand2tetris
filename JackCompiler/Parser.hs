module Parser where

import AST
import qualified Text.Parsec as Ps

commentLineP :: Ps.Parsec String () ()
commentLineP = do
  Ps.spaces
  _ <- Ps.string "//"
  _ <- Ps.manyTill Ps.anyChar (Ps.try $ Ps.oneOf "\n\r\t")
  Ps.spaces
  return ()

commentBlockP :: Ps.Parsec String () ()
commentBlockP = do
  Ps.spaces
  _ <- Ps.string "/*"
  _ <- Ps.manyTill Ps.anyChar (Ps.try $ Ps.string "*/")
  Ps.spaces
  return ()

commentsP :: Ps.Parsec String () ()
commentsP = do
  Ps.spaces
  _ <- Ps.many $ Ps.choice $ map Ps.try [commentLineP, commentBlockP]
  Ps.spaces
  return ()

wrapEscapes :: (Ps.Parsec String () a) -> (Ps.Parsec String () a)
wrapEscapes p = do
  {- Allow spaces or comments on either side of parser's target -}
  _ <- commentsP
  out <- p
  _ <- commentsP
  return out

chooseLit :: [String] -> Ps.Parsec String () String
chooseLit xs = wrapEscapes $ Ps.choice [Ps.try $ Ps.string x | x <- xs]

-- Terminal element parsers
symP :: String -> Ps.Parsec String () Symbol
symP s = do
  r <- wrapEscapes $ Ps.string s
  return (Symbol r)

symsP :: [String] -> Ps.Parsec String () Symbol
symsP syms = do
  sym <- chooseLit syms
  return (Symbol sym)

keywordsP :: [String] -> Ps.Parsec String () Keyword
keywordsP xs = do
  kw <- chooseLit xs
  return (Keyword kw)

keywordP :: String -> Ps.Parsec String () Keyword
keywordP kw = do
  pkw <- wrapEscapes $ Ps.string kw
  return (Keyword pkw)

opP :: Ps.Parsec String () Term
opP = do
  op <- symsP ["+", "-", "*", "/", "&", "|", "<", ">", "="]
  return (Op op)

identifierP :: Ps.Parsec String () Identifier
identifierP = do
  {- a seq of letters, digits, and '_' not starting with a digit -}
  Ps.spaces
  x <- Ps.choice [Ps.letter, Ps.char '_']
  xs <- Ps.many $ Ps.choice [Ps.digit, Ps.letter, Ps.char '_']
  Ps.spaces
  return (Identifier $ [x] ++ xs)

-- Expr parsers
buildExprTree :: Term -> [(Term, Term)] -> (Tree Term)
buildExprTree t [] = (Leaf t)
buildExprTree t (x:xs) = (Node (Leaf t) (fst x)
                           (buildExprTree (snd x) xs))

opTermP :: Ps.Parsec String () (Term, Term)
opTermP = do
  op <- wrapEscapes opP
  t <- wrapEscapes termP
  return (op, t)

exprP :: Ps.Parsec String () Expr
exprP = do
  t <- wrapEscapes termP
  opTerms <- wrapEscapes $ Ps.many opTermP
  return (Expr (buildExprTree t opTerms))

-- SubCall parsers
subCallNameP :: Ps.Parsec String () SubCall
subCallNameP = do
  scn <- wrapEscapes identifierP
  _ <- symP "("
  exprList <- wrapEscapes $ Ps.sepBy exprP $ Ps.string ","
  _ <- symP ")"
  return (SubCallName scn exprList)

subCallClassOrVarP :: Ps.Parsec String () SubCall
subCallClassOrVarP = do
  n <- identifierP
  _ <- symP "."
  sn <- identifierP
  _ <- symP "("
  exprList <- wrapEscapes $ Ps.sepBy exprP $ Ps.string ","
  _ <- symP ")"
  return (SubCallClassOrVar n sn exprList)

-- Term parsers
keywordConstantP :: Ps.Parsec String () Term
keywordConstantP = do
  kwc <- chooseLit ["true", "false", "null", "this"]
  return (KeywordConstant kwc)

unaryOpP :: Ps.Parsec String () Term
unaryOpP = do
  uop <- symsP ["-", "~"]
  t <- termP
  return (UnaryOp uop t)

integerConstantP :: Ps.Parsec String () Term
integerConstantP = do
  -- use of read safe because only ascii digits 0-9 make it past the parser
  i <- fmap read $ Ps.many1 Ps.digit
  return (IntegerConstant i)

stringConstantP :: Ps.Parsec String () Term
stringConstantP = do
  s <- Ps.between (Ps.char '"') (Ps.char '"') (Ps.many1 $ Ps.noneOf ['"', '\n'])
  return (StringConstant s)

varNameP :: Ps.Parsec String () Term
varNameP = do
  vn <- identifierP
  return (VarName vn)

varNameExprP :: Ps.Parsec String () Term
varNameExprP = do
  vn <- identifierP
  _ <- symP "["
  expr <- exprP
  _ <- symP "]"
  return (VarNameExpr vn expr)

parenExprP :: Ps.Parsec String () Term
parenExprP = do
  _ <- symP "("
  expr <- exprP
  _ <- symP ")"
  return (ParenExpr expr)

subroutineCallP :: Ps.Parsec String () Term
subroutineCallP = do
  sc <- Ps.choice $ map Ps.try [subCallNameP, subCallClassOrVarP]
  return (SubroutineCall sc)

termP :: Ps.Parsec String () Term
termP = do
  {-
    Order matters here, try the more selective parsers first to avoid a
    partial parse of an input like foo.bar()
  -}
  t <- Ps.choice $ map Ps.try [varNameExprP, subroutineCallP, integerConstantP,
                               stringConstantP, keywordConstantP, varNameP,
                               unaryOpP, parenExprP]
  return t

-- Statement parsers
letVarNameP :: Ps.Parsec String () LetVarName
letVarNameP = do
  lvn <- Ps.choice $ map Ps.try [varNameExprP, varNameP]
  return (vnToLetVn lvn)
  where vnToLetVn (VarName i) = (LetVarName i)
        vnToLetVn (VarNameExpr i expr) = (LetVarNameExpr i expr)

letP :: Ps.Parsec String () Statement
letP = do
  _ <- keywordP "let"
  lvn <- letVarNameP
  _ <- symP "="
  expr <- exprP
  _ <- symP ";"
  return (Let lvn expr)

elseP :: Ps.Parsec String () Else
elseP = do
  _ <- keywordP "else"
  _ <- symP "{"
  stmts <- Ps.many statementP
  _ <- symP "}"
  return (Else stmts)

ifP :: Ps.Parsec String () Statement
ifP = do
  _ <- keywordP "if"
  _ <- symP "("
  expr <- exprP
  _ <- symP ")"
  _ <- symP "{"
  stmts <- Ps.many statementP
  _ <- symP "}"
  els <- Ps.optionMaybe elseP
  return (If expr stmts els)

whileP :: Ps.Parsec String () Statement
whileP = do
  _ <- keywordP "while"
  _ <- symP "("
  expr <- exprP
  _ <- symP ")"
  _ <- symP "{"
  stmts <- Ps.many statementP
  _ <- symP "}"
  return (While expr stmts)

doP :: Ps.Parsec String () Statement
doP = do
  _ <- keywordP "do"
  (SubroutineCall subCall) <- subroutineCallP
  _ <- symP ";"
  return (Do subCall)

returnP :: Ps.Parsec String () Statement
returnP = do
  _ <- keywordP "return"
  expr <- Ps.optionMaybe exprP
  _ <- symP ";"
  return (Return expr)

statementP :: Ps.Parsec String () Statement
statementP = do
  s <- Ps.choice $ map Ps.try [letP, ifP, whileP, doP, returnP]
  return s

-- Program structure parsers
typeOrKwP :: [String] -> Ps.Parsec String () Type
typeOrKwP kws = do
  tName <- Ps.choice $ map Ps.try [tParseKWs, tIdentifierP]
  return tName
  -- the type juggling below is done to get the same return type from
  -- keywordsP and identifierP so they can be used with Ps.choice
  where tParseKWs = do
          kw <- keywordsP kws
          return (TKeyword kw)
        tIdentifierP = do
          i <- identifierP
          return (TIdentifier i)

varDecP :: Ps.Parsec String () VarDec
varDecP = do
  _ <- keywordP "var"
  typ <- (typeOrKwP ["int", "char", "boolean"])
  varN <- identifierP
  Ps.spaces
  Ps.optional $ Ps.string ","
  -- varNList is dropping all the ',' symbols, you'll need to account for
  -- this in the xml generation
  varNList <- wrapEscapes $ Ps.sepBy identifierP $ Ps.string ","
  _ <- symP ";"
  return (VarDec typ varN varNList)

subroutineBodyP :: Ps.Parsec String () SubroutineBody
subroutineBodyP = do
  _ <- symP "{"
  varDecs <- Ps.many varDecP
  statments <- Ps.many statementP
  _ <- symP "}"
  return (SubroutineBody varDecs statments)

paramListP :: Ps.Parsec String () ParameterList
paramListP = do
  params <- wrapEscapes $ Ps.sepBy paramP $ Ps.string ","
  return (ParameterList params)
  where paramP = do
          typ <- (typeOrKwP ["int", "char", "boolean"])
          Ps.spaces
          varN <- identifierP
          return (typ, varN)

subroutineDecP :: Ps.Parsec String () SubroutineDec
subroutineDecP = do
  kw <- keywordsP ["constructor", "function", "method"]
  retType <- typeOrKwP ["void"]
  subN <- identifierP
  _ <- symP "("
  params <- paramListP
  _ <- symP ")"
  subBody <- subroutineBodyP
  return (SubroutineDec kw retType subN params subBody)

classVarDecP :: Ps.Parsec String () ClassVarDec
classVarDecP = do
  kw <- keywordsP ["static", "field"]
  typ <- (typeOrKwP ["int", "char", "boolean"])
  varN <- identifierP
  Ps.spaces
  Ps.optional $ Ps.string ","
  -- varNList is dropping all the ',' symbols, you'll need to account for
  -- this in the xml generation
  varNList <- wrapEscapes $ Ps.sepBy identifierP $ Ps.string ","
  _ <- symP ";"
  return (ClassVarDec kw typ varN varNList)

classP :: Ps.Parsec String () Class
classP = do
  cls <- keywordsP ["class"]
  clsName <- identifierP
  _ <- symP "{"
  clsVarDecs <- Ps.many classVarDecP
  subRDecs <- Ps.many subroutineDecP
  _ <- symP "}"
  return (Class cls clsName clsVarDecs subRDecs)

parseJack :: String -> Either Ps.ParseError Class
parseJack f = Ps.parse classP f f
