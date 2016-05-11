module ParserSpec where

import Parser.Parser
import Data.DataType

import Test.Hspec
import Test.QuickCheck hiding (Args)
import Control.Exception(evaluate)

extractValue (Right a) = a

testRead = extractValue . parseExpr

test a b = testRead a `shouldBe` b

testSingle a = test (a ++ " End")

testString a b = show (testRead (a++ " End")) `shouldBe` b

exProgram ="Pro    Decl x w End    LetBe x Num 1 End End    Func g Para z End        Pro            Return Plus Id x End Id z End End End        End    End    Func f Para y End        Pro            Decl x End            LetBe x Plus Id y End Num 1 End End End            Return                AppFun g                     List Mult Id y End Id x End End End                End            End        End    End    Read w End    Print AppFun f List Id w End End End End End "

addPro s = concat ["Pro ", s, " End"]

testAST a b = extractValue  (getAST "" (addPro a))  `shouldBe` b

s0 = Slot 0
s1 = Slot 1
s2 = Slot 2
s3 = Slot 3

var = Value . Id

spec :: Spec
spec  = do
  describe "parse a normal expression" $ do
    it "parse a number" $ do
      testSingle "Num 123" (Num 123)
    it "pares a variable" $ do
      testString "Id x" "Id x"
    it "parse function application" $ do
      testString "AppFun f List Id x End Num 2 End End" "AppFun f [Id x,Num 2]"
    it "parse arithmatics" $ do
      testString "Minus Num 2 End Num 3 End" "Minus (Num 2) (Num 3)"

  describe "parse a program" $ do
    it "parse whole program" $ do
      case (parseProgram "" exProgram) of
        Right _ -> True
        Left err -> error (show err)

  describe "after a rename pass" $ do
    it "first level rename" $ do
      testAST "Decl x End" [Decl [s0]]
      testAST "Decl x End LetBe x Num 2 End End" [Decl [s0], LetBe s0 (Num 2)]
      testAST "Decl x y End Id y End Id x End" [Decl [s0, s1], Value (Id s1), Value (Id s0)]

    it "stactic scope" $ do
      testAST "Decl x End Func g Para z End Pro Id x End End End"
        [Decl [s0], Func s1 [s2] [var s0]]
      testAST "Decl x End Func g Para x End Pro Id x End End End"
        [Decl [s0], Func s1 [s2] [var s2]]
