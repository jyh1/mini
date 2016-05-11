module Parser.Parser where

import Data.DataType
import Parser.Lexer

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Pos

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

-- binary s f  = Ex.Infix (reservedOp s >> return f )
--
-- table = [[binary "*" Mult Ex.AssocLeft,
--           binary "/" Div Ex.AssocLeft]
--         ,[binary "+" Plus Ex.AssocLeft,
--           binary "-" Minus Ex.AssocLeft]]

parseVar :: Parser Var
parseVar = do
  name <- identifier
  pos <- getPosition
  return (Var name pos)

end = reserved "End"
endWith n = end  >> n
endReturn n = endWith (return n)

num :: Parser Expr
num = do
  reserved "Num"
  n <- integer
  endReturn (Num n)

var :: Parser Expr
var = do
  reserved "Id"
  name <- parseVar
  endReturn (Id name)

appFun :: Parser Expr
appFun = do
  reserved "AppFun"
  name <- parseVar
  args  <- argList
  endReturn (AppFun name args)

arithmatic :: String ->  (Expr -> Expr -> Expr)
   -> Parser Expr
arithmatic name cons = do
  reserved name
  e1 <- expr
  e2 <- expr
  endReturn (cons e1 e2)

arithmatics = foldr1 (<|>) ops
  where ops = zipWith arithmatic ["Plus", "Minus", "Mult"]
                                        [Plus, Minus, Mult]

argList :: Parser ArgList
argList = do
  reserved "List"
  lis <- many1 expr
  endReturn lis

expr :: Parser Expr
expr = num
  <|> var
  <|> appFun
  <|> arithmatics

parseExpr :: String -> Either ParseError Expr
parseExpr  = parse expr "miniInterpreter"

decl :: Parser Command
decl = do
  reserved "Decl"
  vars <- varList
  endReturn (Decl vars)

value :: Parser Command
value = fmap Value expr

func :: Parser Command
func = do
  reserved "Func"
  name <- parseVar
  paras <- paraList
  pros <- program
  endReturn (Func name paras pros)

letBe :: Parser Command
letBe = do
  reserved "LetBe"
  var <- parseVar
  ex <- expr
  endReturn (LetBe var ex)

runFun :: Parser Command
runFun = do
  reserved "RunFun"
  name <- parseVar
  args <- argList
  endReturn (RunFun name args)

simple :: (String, Expr -> Command)
   -> Parser Command
simple (name, f) = do
  reserved name
  ex <- expr
  endReturn (f ex)

returnPrint = foldr1 (<|>) (map simple
                                                [("Return", Return),
                                                 ("Print", Print)]  )

rea :: Parser Command
rea  = do
  reserved "Read"
  name <- parseVar
  endReturn (Read name)


command :: Parser Command
command = foldr1 (<|>)
  [decl, value, func, letBe, runFun, returnPrint, rea]

program :: Parser Program
program = do
  reserved "Pro"
  cs <- many command
  endReturn cs

paraList :: Parser ParaList
paraList = do
  reserved "Para"
  vars <- varList
  endReturn vars

varList :: Parser [Var]
varList = many1 parseVar

parseProgram :: String -> Either ParseError Program
parseProgram  = parse program "mini parser"
