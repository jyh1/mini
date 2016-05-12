module Parser.Parser
  (parseFile, getAST, parseProgram, parseExpr)
where

import Data.DataType
import Parser.Lexer
import Parser.Rename

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Pos

import Control.Monad.Except

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

-- binary s f  = Ex.Infix (reservedOp s >> return f )
--
-- table = [[binary "*" Mult Ex.AssocLeft,
--           binary "/" Div Ex.AssocLeft]
--         ,[binary "+" Plus Ex.AssocLeft,
--           binary "-" Minus Ex.AssocLeft]]

parseVar :: Parser LitVar
parseVar = do
  name <- identifier
  pos <- getPosition
  return (Var name pos)

end = reserved "End"
endWith n = end  >> n
endReturn n = endWith (return n)

num :: Parser LitExpr
num = do
  reserved "Num"
  n <- integer
  endReturn (Num n)

var :: Parser LitExpr
var = do
  reserved "Id"
  name <- parseVar
  endReturn (Id name)

appFun :: Parser LitExpr
appFun = do
  reserved "AppFun"
  name <- parseVar
  args  <- argList
  endReturn (AppFun name args)

arithmatic :: String ->  (LitExpr -> LitExpr -> LitExpr)
   -> Parser LitExpr
arithmatic name cons = do
  reserved name
  e1 <- expr
  e2 <- expr
  endReturn (cons e1 e2)

arithmatics = foldr1 (<|>) ops
  where ops = zipWith arithmatic ["Plus", "Minus", "Mult"]
                                        [Plus, Minus, Mult]

argList :: Parser (ArgList LitVar)
argList = do
  reserved "List"
  lis <- many expr
  endReturn lis

expr :: Parser LitExpr
expr = num
  <|> var
  <|> appFun
  <|> arithmatics

parseExpr :: String -> Either ParseError (Expr LitVar)
parseExpr  = parse expr "miniInterpreter"

decl :: Parser LitCommand
decl = do
  reserved "Decl"
  vars <- varList
  endReturn (Decl vars)

value :: Parser LitCommand
value = fmap Value expr

func :: Parser LitCommand
func = do
  reserved "Func"
  name <- parseVar
  paras <- paraList
  pros <- program
  endReturn (Func name paras pros)

letBe :: Parser LitCommand
letBe = do
  reserved "LetBe"
  var <- parseVar
  ex <- expr
  endReturn (LetBe var ex)

runFun :: Parser LitCommand
runFun = do
  reserved "RunFun"
  name <- parseVar
  args <- argList
  endReturn (RunFun name args)

simple :: (String, LitExpr -> LitCommand)
   -> Parser LitCommand
simple (name, f) = do
  reserved name
  ex <- expr
  endReturn (f ex)

returnPrint = foldr1 (<|>) (map simple
                                                [("Return", Return),
                                                 ("Print", Print)]  )

rea :: Parser LitCommand
rea  = do
  reserved "Read"
  name <- parseVar
  endReturn (Read name)


command :: Parser LitCommand
command = foldr1 (<|>)
  [decl, value, func, letBe, runFun, returnPrint, rea]

program :: Parser LitProgram
program = do
  reserved "Pro"
  cs <- many command
  endReturn cs

paraList :: Parser (ParaList LitVar)
paraList = do
  reserved "Para"
  vars <- varList
  endReturn vars

varList :: Parser [LitVar]
varList = many parseVar

toStage :: Either ParseError a -> Stage a
toStage (Right a) = Right a
toStage (Left err) = throwError (Parser err)

parseProgram :: String -> String -> Either ParseError LitProgram
parseProgram  = parse program

getAST :: String -> String -> Stage RenamedProgram
getAST name = (>>=  runRename) . toStage . parseProgram name

parseFile :: FilePath -> IO (Stage RenamedProgram)
parseFile file = do
  pro <- readFile file
  return $ getAST file pro
