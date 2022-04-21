module Parser where

import AST
import qualified Text.Parsec as Ps

wrapSps :: (Ps.Parsec String () a) -> (Ps.Parsec String () a)
wrapSps p = do
  {- Allow spaces on either side of parser's target -}
  Ps.spaces
  out <- p
  Ps.spaces
  return out

chooseLit :: [String] -> Ps.Parsec String () String
chooseLit xs = wrapSps $ Ps.choice [Ps.try $ Ps.string x | x <- xs]

-- Terminal element parsers
symP :: String -> Ps.Parsec String () Symbol
symP s = do
  r <- wrapSps $ Ps.string s
  return (Symbol r)

keywordsP :: [String] -> Ps.Parsec String () Keyword
keywordsP xs = do
  kw <- chooseLit xs
  return (Keyword kw)

keywordP :: Ps.Parsec String () Keyword
keywordP = do
  kw <- chooseLit ["class", "constructor", "function", "method", "field",
                   "static", "var", "int", "char", "boolean", "void", "true",
                   "false", "null", "this", "let", "do", "if", "else", "while",
                   "return"]
  return (Keyword kw)

symbolP :: Ps.Parsec String () Symbol
symbolP = do
  sy <- chooseLit ["{", "}", "(", ")", "[", "]", ".", ",", ";",
                   "+", "-", "*", "/", "&", "|", "<", ">", "=", "~"]
  return (Symbol sy)

opP :: Ps.Parsec String () Op
opP = do
  op <- chooseLit ["+", "-", "*", "/", "&", "|", "<", ">", "="]
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
opTermP :: Ps.Parsec String () (Op, Term)
opTermP = do
  op <- wrapSps opP
  t <- wrapSps termP
  return (op, t)

exprP :: Ps.Parsec String () Expr
exprP = do
  t <- wrapSps termP
  opTerms <- wrapSps $ Ps.many opTermP
  return (Expr t opTerms)

-- SubCall parsers
subCallNameP :: Ps.Parsec String () SubCall
subCallNameP = do
  scn <- wrapSps identifierP
  lp <- symP "("
  exprList <- wrapSps $ Ps.sepBy exprP $ Ps.string ","
  rp <- symP ")"
  return (SubCallName scn lp exprList rp)

subCallClassOrVarP :: Ps.Parsec String () SubCall
subCallClassOrVarP = do
  n <- identifierP
  dot <- Ps.string "."
  sn <- identifierP
  lp <- symP "("
  exprList <- wrapSps $ Ps.sepBy exprP $ Ps.string ","
  rp <- symP ")"
  return (SubCallClassOrVar n (Symbol ".") sn lp exprList rp)

-- Term parsers
keywordConstantP :: Ps.Parsec String () Term
keywordConstantP = do
  kwc <- chooseLit ["true", "false", "null", "this"]
  return (KeywordConstant kwc)

unaryOpP :: Ps.Parsec String () Term
unaryOpP = do
  uop <- chooseLit ["-", "~"]
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
  lt <- symP "let"
  lvn <- letVarNameP
  eq <- symP "="
  expr <- exprP
  sc <- symP ";"
  return (Let lt lvn eq expr sc)

elseP :: Ps.Parsec String () Else
elseP = do
  els <- symP "else"
  lc <- symP "{"
  stmts <- Ps.many statementP
  rc <- symP "}"
  return (Else els lc stmts rc)

ifP :: Ps.Parsec String () Statement
ifP = do
  i <- symP "if"
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
  wh <- symP "while"
  lp <- symP "("
  expr <- exprP
  rp <- symP ")"
  lc <- symP "{"
  stmts <- Ps.many statementP
  rc <- symP "}"
  return (While wh lp expr rp lc stmts rc)

doP :: Ps.Parsec String () Statement
doP = do
  d <- symP "do"
  (SubroutineCall subCall) <- subroutineCallP
  return (Do d subCall)

returnP :: Ps.Parsec String () Statement
returnP = do
  rt <- symP "return"
  expr <- Ps.optionMaybe exprP
  sc <- symP ";"
  return (Return rt expr sc)

statementP :: Ps.Parsec String () Statement
statementP = do
  s <- Ps.choice $ map Ps.try [letP, ifP, whileP, doP, returnP]
  return s

main :: IO ()
main = do
  let blarg = Ps.parse identifierP "error file" "hah ahhaa4"
  print blarg
