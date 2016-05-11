module Data.DataType where

import Text.Parsec.Pos
import Control.Monad.Except
import Text.Parsec
-- import Control.Monad.Trans.Except



data Var = Var String SourcePos
  | Slot  Int
    deriving(Eq)

instance Show Var where
  show (Slot n) = "#" ++ show n
  show (Var n _) = n

data Expr = Num Integer
  | Id Var
  | AppFun Var ArgList
  | Plus Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
    deriving (Show, Eq)
type ArgList = [Expr]

type ParaList = [Var]

data Command = Decl [Var]
  | Value Expr
  | Func Var ParaList Program
  | LetBe Var Expr
  | RunFun Var ArgList
  | Return Expr
  | Read Var
  | Print Expr
    deriving (Show, Eq)
type Program = [Command]


data Error = Parser ParseError
  | NotInScope String SourcePos
  | NameCollition String SourcePos
    deriving (Show)


type Stage = Either Error
