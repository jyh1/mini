{-#LANGUAGE TemplateHaskell#-}
module Eval.Eval(runProgram) where

import Data.DataType

import Control.Lens
import qualified Data.IntMap.Strict as I
import Control.Monad.State
import Data.Maybe

data Env = Env { _varTable :: Record
  , _output :: [String]
  , _returned :: Bool
  , _input :: [String]
}

makeLenses '' Env

nullEnv = Env I.empty [] False

type EvalState = StateT Env Stage

type EvalResult = EvalState Result

evalVar :: IntVar -> EvalResult
evalVar (Slot n _ _) = uses varTable (fromJust . I.lookup n)

setVar :: IntVar -> Result -> EvalState ()
setVar (Slot n _ _) res = varTable %= I.insert n res

declareVar :: IntVar -> EvalState ()
declareVar v = setVar v None

applyFun :: Result -> [Result] -> EvalResult
applyFun (Lambda lis pro) argLis
  | length lis /= length argLis = throw FunError
  | otherwise = do
    let nlis = map unpackLit lis
    varTable %= I.union (I.fromList (zip nlis argLis))
    evalProgram pro

evalExpr :: RenamedExpr -> EvalResult
evalExpr (Num n) = return $ Int n
evalExpr (Id var) = evalVar var
evalExpr (AppFun var paras) = do
  fun <- evalVar var
  rs <- mapM evalExpr paras
  applyFun fun rs
evalExpr (Plus e1 e2) = evalArith (+) e1 e2
evalExpr (Minus e1 e2) = evalArith (-) e1 e2
evalExpr (Mult e1 e2) = evalArith (*) e1 e2
evalExpr (Div e1 e2) = evalArith div e1 e2

evalArith :: (Integer -> Integer -> Integer)
   -> RenamedExpr -> RenamedExpr
    -> EvalResult
evalArith f e1 e2 = do
  (Int n1) <- evalToInt e1
  (Int n2) <- evalToInt e2
  return (Int (f n1 n2))

evalToInt :: RenamedExpr -> EvalResult
evalToInt e = do
  ne <- evalExpr e
  case ne of
    Int _ -> return ne
    _ -> throw (ExpectedInt ne)

evalToLambda :: RenamedExpr -> EvalResult
evalToLambda e = do
  ne <- evalExpr e
  case ne of
    Lambda _ _ -> return ne
    _ -> throw (ExpectedInt ne)

noneRes = return None

evalCommand :: RenamedCommand -> EvalResult
evalCommand (Decl vs) = mapM_ declareVar vs >> noneRes
evalCommand (Value expr) = evalExpr expr
evalCommand (Func var pars pros) = setVar var (Lambda pars pros) >> noneRes
evalCommand (LetBe v e) = do
  ne <- evalExpr e
  setVar v ne
  noneRes
evalCommand (RunFun f args) = evalExpr (AppFun f args)
evalCommand (Return e) = do
    res <- evalExpr e
    returned .= True
    return res
evalCommand (Read e) = readBuffer >>= setVar e >> noneRes
evalCommand (Print expr) = do
  e <- evalExpr expr
  printBuffer e

readBuffer :: EvalResult
readBuffer = do
  buff <- use input
  case buff of
    [] -> throw EOF
    i:_ -> do
      input %= tail
      tryRead i

tryRead :: String -> EvalResult
tryRead s =
  case reads s of
    [(n, _)] -> return (Int n)
    _ -> throw ReadError

printBuffer :: Result -> EvalResult
printBuffer (Int n) =
  output %= (show n :) >> noneRes
printBuffer _ = throw PrintError

evalProgram :: RenamedProgram -> EvalResult
evalProgram [] = return (Int 0)
evalProgram (c:cs) = do
  res <- evalCommand c
  ret <- use returned
  if ret then do
      returned .= False
      return res
    else evalProgram cs

runProgram :: RenamedProgram -> [String] -> Stage [String]
runProgram pro input =
  fmap (^. output) (execStateT (evalProgram pro) (nullEnv input))
