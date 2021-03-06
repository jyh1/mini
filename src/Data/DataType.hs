 module Data.DataType where

import Text.Parsec.Pos
import Control.Monad.Except
import Text.Parsec
import qualified Data.IntMap.Strict as I
-- import Control.Monad.Trans.Except



-- data Var = Var String SourcePos
--   | Slot  Int
--     deriving(Eq)

data LitVar = Var String SourcePos
  deriving(Eq)

instance Show LitVar where
  show (Var x _) = x

unpackLit (Slot n _ _) = n

data IntVar = Slot Int String SourcePos
  deriving(Eq)

instance Show IntVar where
  show (Slot _ s _) = s

prettyShow :: IntVar -> String
prettyShow (Slot _ n p) =  n ++ " at " ++ show p

-- instance Show Var where
--   show (Slot n) = "#" ++ show n
--   show (Var n _) = n

data Expr var = Num Integer
  | Id var
  | AppFun var (ArgList var)
  | Plus (Expr var) (Expr var)
  | Minus (Expr var) (Expr var)
  | Mult (Expr var) (Expr var)
  | Div (Expr var) (Expr var)
    deriving (Show, Eq)
type ArgList var = [Expr var]

type ParaList var = [var]

data Command var = Decl [var]
  | Value (Expr var)
  | Func var (ParaList  var) (Program var)
  | LetBe var (Expr var)
  | RunFun var (ArgList var)
  | Return (Expr var)
  | Read var
  | Print (Expr var)
  | Record IntVar (ParaList IntVar) (Program IntVar) [IntVar]
    deriving (Show, Eq)

type Program var = [Command var]

type RenamedExpr = Expr IntVar
type RenamedCommand = Command IntVar
type RenamedProgram = Program IntVar

type LitExpr = Expr LitVar
type LitCommand = Command LitVar
type LitProgram = Program LitVar


data Error = Parser ParseError
  | NotInScope String SourcePos
  | NameCollition String SourcePos
  | ExpectedInt Result
  | ExpectedFunction Result
  | EOF
  | ReadError
  |PrintError
  | FunError

instance Show Error where
  show (Parser err) =show err
  show (NotInScope x pos) = concat ["Varaible ", x, " is not in scope! ", show pos]
  show (NameCollition x pos) = concat ["Name collision: ", x, "! ", show pos]
  show (ExpectedInt _) = "Int is expected!"
  show (ExpectedFunction _) = "Function is expected!"
  show EOF = "Unexpected eof!"
  show ReadError = "Expected an int as input!"
  show PrintError = "Unprintable object!"
  show FunError = "Function is applied to too few or too many arguments!"

type Stage = ExceptT Error IO

type Record = I.IntMap Result

data Result = Int Integer
  | Lambda (ParaList IntVar) RenamedProgram [IntVar]
  | None
    deriving(Show)


throw :: (MonadTrans t, MonadError e m) => e -> t m a
throw = lift . throwError
