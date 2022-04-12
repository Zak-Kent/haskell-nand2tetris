import AST
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)

keywordP :: ReadP Keyword
keywordP = do
  kw <- string "class" <|> string "constructor" <|> string "function"
        <|> string "method" <|> string "field" <|> string "static"
        <|> string "var" <|> string "int" <|> string "char" <|> string "boolean"
        <|> string "void" <|> string "true" <|> string "false" <|> string "null"
        <|> string "this" <|> string "let" <|> string "do" <|> string "if"
        <|> string "else" <|> string "while" <|> string "return"
  return (Keyword kw)

main :: IO ()
main = do
  let (p, _) = head $ readP_to_S keywordP "class"
  print p
