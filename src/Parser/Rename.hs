{-#LANGUAGE TemplateHaskell #-}
module Parser.Rename
  (runRename)
where

import Data.DataType

import Control.Lens
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except


data Buffer = Buffer { _counter :: Int
  , _upper :: M.Map String Int
  , _current :: M.Map String Int
  , _vars :: [IntVar]
}


makeLenses '' Buffer

initialBuffer = Buffer 0 M.empty M.empty []

type Rename = StateT Buffer Stage

declVar :: LitVar  -> Rename IntVar
declVar (Var x pos) = do
  test <- uses current (M.member x)
  if test
    then throw (NameCollition x pos)
    else do
      now <- use counter
      counter += 1
      current %=  M.insert x now
      let newV = Slot now x pos
      vars %= (newV :)
      return newV

useVar :: LitVar -> Rename IntVar
useVar (Var x pos) = do
  cur  <- use current
  upp <- use upper
  let find = M.lookup x
  case (find cur, find upp) of
    (Just a, _) -> return (Slot a x pos)
    (Nothing, Just a) -> return (Slot a x pos)
    _ -> throw (NotInScope x pos)

-- | restore state after exec the action
restore :: Rename a -> Rename a
restore ac = do
  old <- get
  res <- ac
  newCont <- use counter
  put (counter .~ newCont $ old)
  return res

descend :: Rename ()
descend = do
  cur <- use current
  upper %= M.union cur
  current .= M.empty
  vars .= []

renameExpr :: LitExpr -> Rename RenamedExpr
renameExpr (Id var) = fmap Id (useVar var)
renameExpr (AppFun fun args) = do
  newf <- useVar fun
  newArgs <- mapM renameExpr args
  return (AppFun newf newArgs)
renameExpr (Plus e1 e2) = renameTwo Plus e1 e2
renameExpr (Minus e1 e2) = renameTwo Minus e1 e2
renameExpr (Mult e1 e2) = renameTwo Mult e1 e2
renameExpr (Div e1 e2) = renameTwo Div e1 e2
renameExpr (Num n) = return (Num n)


renameTwo f e1 e2 = do
  newE1 <- renameExpr e1
  newE2 <- renameExpr e2
  return (f newE1 newE2)

renameCommand :: LitCommand -> Rename RenamedCommand
renameCommand (Decl vars) =   fmap Decl (mapM declVar vars)
renameCommand (Value ex) =  fmap Value (renameExpr ex)
renameCommand (Func f paras pro) =  do
  newF <- declVar f
  restore $ do
    descend
    newParas <- mapM declVar paras
    newPro <- renameProgram pro
    freed <- use vars
    return $ Record newF newParas newPro freed
renameCommand (LetBe var ex) =  do
  newVar <- useVar var
  newEx <- renameExpr ex
  return (LetBe newVar newEx)
renameCommand (RunFun fun args) =  do
  newFun <- useVar fun
  newArgs <- mapM renameExpr args
  return (RunFun newFun newArgs)
renameCommand (Return ex) = fmap Return (renameExpr ex)
renameCommand (Read var) = fmap Read (useVar var)
renameCommand (Print ex) = fmap Print (renameExpr ex)

renameProgram ::LitProgram -> Rename RenamedProgram
renameProgram = mapM renameCommand

runRename :: LitProgram -> Stage RenamedProgram
runRename pro = evalStateT (renameProgram pro) initialBuffer
