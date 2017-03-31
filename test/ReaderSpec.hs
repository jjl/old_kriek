module ReaderSpec where

import Kriek.Ast
import Kriek.Reader
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = hspec spec

parseForm :: String -> Either (ParseError (Token String) Dec) AST
parseForm s = case parse form "(test)" s of
  Right (Form ast _ _) -> Right ast
  Left x -> Left x

-- Returns a list of forms using type t
--
-- Useful when testing AList
formsFrom :: (t -> AST) -> [t] -> [Form]
formsFrom t = map (emptyForm . t)

-- Returns a form with no Meta or Position
emptyForm :: AST -> Form
emptyForm x = Form x Nothing Nothing

-- Surround a string with another string.
surround :: String -> String -> String
surround c str = c ++ str ++ c

spec :: Spec
spec = do
  describe "symbols" $ do
    it "parses single characters" $
      parseForm "a" `shouldParse` ASymbol (Name "a")
    it "parses full words" $
      parseForm "caramel" `shouldParse` ASymbol (Name "caramel")
    it "parses combinations of letters and numbers" $
      parseForm "a1b2" `shouldParse` ASymbol (Name "a1b2")
    it "fails when a digit is used as a first character" $
      parse form "1de" `shouldFailOn` "1"
    it "parses kebab case" $
      parseForm "ice-cream" `shouldParse` ASymbol (Name "ice-cream")
    it "parses namespaces" $
      parseForm "chocolate/dark" `shouldParse` ASymbol (NSName "chocolate" "dark")

  describe "quasi-symbols" $ do
    it "parses single characters" $
      parseForm "|a|" `shouldParse` ASymbol (Name "a")
    it "parses full words" $
      parseForm "|caramel|" `shouldParse` ASymbol (Name "caramel")
    it "parses combinations of letters and numbers" $
      parseForm "|a1b2|" `shouldParse` ASymbol (Name "a1b2")
    it "fails when a digit is used as a first character" $
      parse form "|1de|" `shouldFailOn` "1"
    it "parses kebab case" $
      parseForm "|ice-cream|" `shouldParse` ASymbol (Name "ice-cream")
    it "parses namespaces" $
      parseForm "|chocolate/dark|" `shouldParse` ASymbol (NSName "chocolate" "dark")
    it "allows spaces" $
      let sym = "chocolate chip  ice cream"
      in parseForm (surround "|" sym) `shouldParse` ASymbol (Name sym)
    it "fails on newline" $
      parse form "|a\nnewline|" `shouldFailOn` "\n"
    it "fails when not closed" $
      parse form "|not-finished" `shouldFailOn` "d"

  describe "integers" $ do
    it "parses single digits" $
      parseForm "7" `shouldParse` AInt 7
    it "parses multiple digits" $
      parseForm "823" `shouldParse` AInt 823
    it "parses large values" $
      parseForm "7381319930213821312" `shouldParse` AInt 7381319930213821312

  describe "decimals" $ do
    it "parses basic decimals" $
      parseForm "1.2" `shouldParse` AFloat 1.2
    it "parses large values" $
      parseForm "321.37543832175493213121" `shouldParse` AFloat 321.37543832175493213121
    it "works with scientific notation (positive)" $
      parseForm "9.81e+23" `shouldParse` AFloat 9.81e+23
    it "works with scientific notation (negative)" $
      parseForm "2.15e-10" `shouldParse` AFloat 2.15e-10

  describe "keywords" $ do
    it "parses single characters" $
      parseForm ":a" `shouldParse` AKeyword "a"
    it "parses full words" $
      parseForm ":cats" `shouldParse` AKeyword "cats"
    it "parses combinations of letters and numbers" $
      parseForm ":b12" `shouldParse` AKeyword "b12"
    it "fails when a digit is used as a first character" $
      parse form ":12daysof" `shouldFailOn` "1"
    it "parses kebab case" $
      parseForm ":cats-and-reddit" `shouldParse` AKeyword "cats-and-reddit"
    it "fails with namespaces" $
      parse form ":my-ns/your-var" `shouldFailOn` "/"

  it "parses nil" $ parseForm "nil" `shouldParse` ANil

  describe "characters" $ do
    it "parses plain characters" $
      parseForm "#\\a" `shouldParse` AChar 'a'
    it "parses numbers as characters" $
      parseForm "#\\1" `shouldParse` AChar '1'

  describe "strings" $ do
    it "parses empty strings" $
      parseForm "\"\"" `shouldParse` AString ""
    it "parses single characters" $
      parseForm "\"a\"" `shouldParse` AString "a"
    -- FIXME: generate a large block
    it "parses large blocks" $
      let txt = "This is a large block of text. Filler filler filler"
      in parseForm (surround "\"" txt) `shouldParse` AString txt
    it "escapes special characters" $
      parseForm "\"A\\nNew\\tLine\"" `shouldParse` AString "A\nNew\tLine"

  describe "tuples" $ do
    it "parses an empty tuple" $
      parseForm "[]" `shouldParse` ATuple []
    -- TODO: Use QuickCheck for generating N items
    it "accepts N number of items" $
      parseForm "[1 2 3 4 5]" `shouldParse` ATuple (formsFrom AInt [1, 2, 3, 4, 5])
    it "accepts values of any type" $
      let expected = ATuple [emptyForm (AInt 1),
                             emptyForm (ASymbol (Name "a")),
                             emptyForm (AChar 'b'),
                             emptyForm (AString "cee"),
                             emptyForm (AKeyword "d")]
      in parseForm "[1 a 'b' \"cee\" :d]" `shouldParse` expected
    it "accepts nested values" $
      let expected = ATuple [emptyForm (ASymbol (Name "a")),
                             emptyForm (ATuple [emptyForm (ASymbol (Name "b")),
                                                emptyForm (ASymbol (Name "c"))])]
      in parseForm "[a [b c]]" `shouldParse` expected
    it "allows arbitrary amounts of surrounding whitespace" $
      parseForm "[ 1    2\n3   ]" `shouldParse` ATuple (formsFrom AInt [1, 2, 3])

  describe "lists" $ do
    it "parses an empty list" $
      parseForm "()" `shouldParse` AList []
    it "accepts N number of items" $
      parseForm "(1 2 3 4 5)" `shouldParse` AList (formsFrom AInt [1, 2, 3, 4, 5])
    it "accepts values of any type" $
      let expected = AList [emptyForm (AInt 1),
                            emptyForm (ASymbol (Name "a")),
                            emptyForm (AChar 'b'),
                            emptyForm (AString "cee"),
                            emptyForm (AKeyword "d")]
      in parseForm "(1 a 'b' \"cee\" :d)" `shouldParse` expected
    it "accepts nested values" $
      let expected = AList [emptyForm (ASymbol (Name "a")),
                            emptyForm (AList [emptyForm (ASymbol (Name "b")),
                                              emptyForm (ASymbol (Name "c"))])]
      in parseForm "(a (b c))" `shouldParse` expected
    it "allows arbitrary amounts of surrounding whitespace" $
      parseForm "( 1    2\n3   )" `shouldParse` AList (formsFrom AInt [1, 2, 3])
