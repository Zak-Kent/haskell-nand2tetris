import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)


-- TODO: will need to break this out into seperate files later on


-- VM language parser

data Command = Arithmetic String | Push | Pop
             deriving (Show)
data MemSegment = Local | Argument | This | That | Constant | Static | Temp | Pointer
                deriving (Show)
type Index = Integer


pushP :: ReadP String
pushP = string "push"

popP :: ReadP String
popP = string "pop"

arithmeticP :: ReadP String
arithmeticP = string "add" <|> string "sub" <|> string "neg" <|> string "eq" <|>
              string "gt" <|> string "lt" <|> string "and" <|> string "or" <|>
              string "not"
