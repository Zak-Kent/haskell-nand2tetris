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

-- identifierP tests
tIdentifierP = TestCase (assertEqual "identifier parse"
                          (Right (Identifier "foo123"))
                          $ parseIt identifierP "foo123")

tIdentifierPSpaces = TestCase (assertEqual "identifierP handles spaces"
                              (Right (Identifier "bar"))
                              $ parseIt identifierP "     bar     ")

tIdentifierPNoDigit =
  TestCase (assertBool "identifierP doesn't allow a digit in first pos"
           (isLeft $ parseIt identifierP "1foo"))

-- TestLists
terminalParserTests =
  TestList [TestLabel "test keywordP" tKeywordP,
            TestLabel "test keywordP fail" tKeywordPFail]

identifierPTests =
  TestList [TestLabel "test identifierP" tIdentifierP,
            TestLabel "test identifierP spaces" tIdentifierPSpaces,
            TestLabel "test identifierP no digit" tIdentifierPNoDigit]

-- run in REPL with: runTestTT <TestList>
