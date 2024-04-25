module Exercises where

{- Exercise:
Ask the user for two numbers and print their sum. -}

addTwo :: IO ()
addTwo = do
  putStrLn "Enter the first number: "
  firstInput <- getLine
  let firstNumber = read firstInput :: Int
  putStrLn "Enter the second number: "
  secondInput <- getLine
  let secondNumber = read secondInput :: Int
  putStrLn $ "The sum is: " ++ show (firstNumber + secondNumber)

{- Exercise:
Given an integer [n], ask the user to repeatedly
guess a number until they get [n]. Tell them if their
guess is higher or lower than expected. -}

guessGame :: Int -> IO ()
guessGame n = do
  putStrLn "Guess the number: "
  guessInput <- getLine
  let guess = read guessInput :: Int
  if guess < n then do
    putStrLn "Too low!"
    guessGame n
  else if guess > n then do
    putStrLn "Too high!"
    guessGame n
  else putStrLn "You got it!"