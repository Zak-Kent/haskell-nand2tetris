import AST
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)

excludeChars :: [Char] -> Char -> Bool
excludeChars xcs c = not $ any (c ==) xcs

chooseLit :: [String] -> ReadP String
chooseLit xs = choice [string x | x <- xs]

keywordP :: ReadP Keyword
keywordP = do
  kw <- chooseLit ["class", "constructor", "function", "method", "field",
                   "static", "var", "int", "char", "boolean", "void", "true",
                   "false", "null", "this", "let", "do", "if", "else", "while",
                   "return"]
  return (Keyword kw)

symbolP :: ReadP Symbol
symbolP = do
  sy <- chooseLit ["{", "}", "(", ")", "[", "]", ".", ",", ";",
                   "+", "-", "*", "/", "&", "|", "<", ">", "=", "~"]
  return (Symbol sy)

integerConstantP :: ReadP IntegerConstant
integerConstantP = do
  -- use of read safe because only ascii digits 0-9 make it past munch
  i <- fmap read $ munch1 isDigit
  return (IntegerConstant i)

stringConstantP :: ReadP StringConstant
stringConstantP = do
  _ <- satisfy (== '"')
  -- TODO: double check the inner " doesn't need to be escaped
  s <- munch $ excludeChars ['"', '\n'] -- all other chars allowed
  _ <- satisfy (== '"')
  return (StringConstant s)

identifierP :: ReadP Identifier
identifierP = do
  {- a seq of letters, digits, and '_' not starting with a digit -}
  x <- satisfy $ not . isDigit
  xs <- munch $ (\c -> isDigit c || isLetter c || ('_' == c))
  return (Identifier $ [x] ++ xs)

main :: IO ()
main = do
  let (p, _) = head $ readP_to_S keywordP "class"
  print p
