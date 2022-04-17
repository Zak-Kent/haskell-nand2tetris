import Data.Either
import Test.HUnit
import qualified Text.Parsec as Ps
import qualified Text.ParserCombinators.Parsec.Error as PsE
import qualified Text.Parsec.Pos as Pos

import AST
import Parser

parseIt :: (Ps.Parsec String () a) -> String ->  Either Ps.ParseError a
parseIt p s = Ps.parse p "test error" s

buildParseError :: String -> String -> (Int, Int) -> Ps.ParseError
buildParseError m sn (l, c) = PsE.newErrorMessage
                            (PsE.Message m)
                            (Pos.newPos sn l c)

getErrorP :: (Ps.Parsec String () a) -> String -> [PsE.Message]
getErrorP p s = PsE.errorMessages $ fromLeft placeHolderErr err
  where err = parseIt p s
        placeHolderErr = buildParseError "default msg" "default src" (1, 1)

-- How to build a Ps.ParseError if you need to check error messages:
-- you'll need a wrapper func to return a Either Ps.ParseError Term
-- tKeywordPFail = TestCase (assertEqual "keyword failure"
--                                   (Left
--                                     (buildParseError "unexpected \"x\"\nexpecting \"-\" or \"~\""
--                                          "test error" (1,1)))
--                                   $ parseIt unaryOpP "x")

tKeywordP = TestCase (assertEqual "keyword parse"
                          (Right (Keyword "class"))
                          $ parseIt keywordP "class")

tKeywordPFail = TestCase (assertBool "keywordP fail"
                         (isLeft $ parseIt keywordP "x"))

tIdentifierP = TestCase (assertEqual "identifier parse"
                          (Right (Identifier "foo"))
                          $ parseIt identifierP "foo")

tIdentifierPSpaces = TestCase (assertEqual "identifierP handles spaces"
                              (Right (Identifier "bar"))
                              $ parseIt identifierP "     bar     ")

terminal_parsers =
  TestList [TestLabel "test keywordP" tKeywordP,
            TestLabel "test keywordP fail" tKeywordPFail,
            TestLabel "test identifierP" tIdentifierP,
            TestLabel "test identifierP spaces" tIdentifierPSpaces]


-- run in REPL with: runTestTT <TestList>
