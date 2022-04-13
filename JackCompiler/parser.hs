import AST
import qualified Text.Parsec as Ps
import Control.Monad.Identity (Identity)

chooseLit :: [String] -> Ps.ParsecT String () Identity String
chooseLit xs = Ps.choice [Ps.string x | x <- xs]

keywordP :: Ps.ParsecT String () Identity Keyword
keywordP = do
  kw <- chooseLit ["class", "constructor", "function", "method", "field",
                   "static", "var", "int", "char", "boolean", "void", "true",
                   "false", "null", "this", "let", "do", "if", "else", "while",
                   "return"]
  return (Keyword kw)

symbolP :: Ps.ParsecT String () Identity Symbol
symbolP = do
  sy <- chooseLit ["{", "}", "(", ")", "[", "]", ".", ",", ";",
                   "+", "-", "*", "/", "&", "|", "<", ">", "=", "~"]
  return (Symbol sy)

integerConstantP :: Ps.Parsec String () IntegerConstant
integerConstantP = do
  -- use of read safe because only ascii digits 0-9 make it past the parser
  i <- fmap read $ Ps.many1 Ps.digit
  return (IntegerConstant i)

stringConstantP :: Ps.Parsec String () StringConstant
stringConstantP = do
  s <- Ps.between (Ps.char '"') (Ps.char '"') (Ps.many1 $ Ps.noneOf ['"', '\n'])
  return (StringConstant s)

identifierP :: Ps.Parsec String () Identifier
identifierP = do
  {- a seq of letters, digits, and '_' not starting with a digit -}
  x <- Ps.noneOf ['0'..'9']
  xs <- Ps.many $ Ps.choice [Ps.digit, Ps.letter, Ps.char '_']
  return (Identifier $ [x] ++ xs)

main :: IO ()
main = do
  let blarg = Ps.parse identifierP "error file" "hah_ahhaa4"
  print blarg
