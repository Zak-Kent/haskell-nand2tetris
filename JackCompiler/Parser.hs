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
  lp <- symP "("
  exprList <- wrapEscapes $ Ps.sepBy exprP $ Ps.string ","
  rp <- symP ")"
  return (SubCallName scn lp exprList rp)

subCallClassOrVarP :: Ps.Parsec String () SubCall
subCallClassOrVarP = do
  n <- identifierP
  dot <- symP "."
  sn <- identifierP
  lp <- symP "("
  exprList <- wrapEscapes $ Ps.sepBy exprP $ Ps.string ","
  rp <- symP ")"
  return (SubCallClassOrVar n dot sn lp exprList rp)

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
  lb <- symP "["
  expr <- exprP
  rb <- symP "]"
  return (VarNameExpr vn lb expr rb)

parenExprP :: Ps.Parsec String () Term
parenExprP = do
  lp <- symP "("
  expr <- exprP
  rp <- symP ")"
  return (ParenExpr lp expr rp)

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
        vnToLetVn (VarNameExpr i lb expr rb) = (LetVarNameExpr i lb expr rb)

letP :: Ps.Parsec String () Statement
letP = do
  lt <- keywordP "let"
  lvn <- letVarNameP
  eq <- symP "="
  expr <- exprP
  sc <- symP ";"
  return (Let lt lvn eq expr sc)

elseP :: Ps.Parsec String () Else
elseP = do
  els <- keywordP "else"
  lc <- symP "{"
  stmts <- Ps.many statementP
  rc <- symP "}"
  return (Else els lc stmts rc)

ifP :: Ps.Parsec String () Statement
ifP = do
  i <- keywordP "if"
  lp <- symP "("
  expr <- exprP
  rp <- symP ")"
  lc <- symP "{"
  stmts <- Ps.many statementP
  rc <- symP "}"
  els <- Ps.optionMaybe elseP
  return (If i lp expr rp lc stmts rc els)

whileP :: Ps.Parsec String () Statement
whileP = do
  wh <- keywordP "while"
  lp <- symP "("
  expr <- exprP
  rp <- symP ")"
  lc <- symP "{"
  stmts <- Ps.many statementP
  rc <- symP "}"
  return (While wh lp expr rp lc stmts rc)

doP :: Ps.Parsec String () Statement
doP = do
  d <- keywordP "do"
  (SubroutineCall subCall) <- subroutineCallP
  sc <- symP ";"
  return (Do d subCall sc)

returnP :: Ps.Parsec String () Statement
returnP = do
  rt <- keywordP "return"
  expr <- Ps.optionMaybe exprP
  sc <- symP ";"
  return (Return rt expr sc)

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
  var <- keywordP "var"
  typ <- (typeOrKwP ["int", "char", "boolean"])
  varN <- identifierP
  Ps.spaces
  Ps.optional $ Ps.string ","
  -- varNList is dropping all the ',' symbols, you'll need to account for
  -- this in the xml generation
  varNList <- wrapEscapes $ Ps.sepBy identifierP $ Ps.string ","
  sc <- symP ";"
  return (VarDec var typ varN varNList sc)

subroutineBodyP :: Ps.Parsec String () SubroutineBody
subroutineBodyP = do
  lc <- symP "{"
  varDecs <- Ps.many varDecP
  statments <- Ps.many statementP
  rc <- symP "}"
  return (SubroutineBody lc varDecs statments rc)

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
  lb <- symP "("
  params <- paramListP
  rb <- symP ")"
  subBody <- subroutineBodyP
  return (SubroutineDec kw retType subN lb params rb subBody)

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
  sc <- symP ";"
  return (ClassVarDec kw typ varN varNList sc)

classP :: Ps.Parsec String () Class
classP = do
  cls <- keywordsP ["class"]
  clsName <- identifierP
  lc <- symP "{"
  clsVarDecs <- Ps.many classVarDecP
  subRDecs <- Ps.many subroutineDecP
  rc <- symP "}"
  return (Class cls clsName lc clsVarDecs subRDecs rc)

parseJack :: String -> Either Ps.ParseError Class
parseJack f = Ps.parse classP f f
