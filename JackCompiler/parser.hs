import AST
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)

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


main :: IO ()
main = do
  let (p, _) = head $ readP_to_S keywordP "class"
  print p
