module Parsing where

import Text.Parsec
import Text.Parsec.String (Parser)

data Exp
  = Num Int
  | Add Exp Exp
  deriving (Show)

expP :: Parser Exp
expP = parenP addP <|> numP

numP :: Parser Exp
numP = do
  n <- digitsP
  return (Num (read n))

digitsP :: Parser String
digitsP = many1 digit

addP :: Parser Exp
addP = do
  e1 <- expP
  spaces
  char '+'
  spaces
  e2 <- expP
  return (Add e1 e2)

parenP :: Parser a -> Parser a
parenP = between (char '(') (char ')')

eval :: Exp -> Int
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2

main :: IO ()
main = do
  input <- getLine
  case parse expP "" input of
    Left err -> print err
    Right exp -> print (eval exp)
