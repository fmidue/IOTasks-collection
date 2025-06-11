{-# LANGUAGE TypeApplications #-}
module Games.HighLowGame where

import Prelude hiding (putStrLn, readLn)
import Test.IOTasks

{- Basic task:
 - Write a simple guessing game, where the goal
 - is to guess a hidden number. After each guess
 - the program gives a hint, indicating whether
 - the hidden number is higher or lower than the
 - guessed number. When the number is guessed the
 - program outputs the number of guesses the user
 - needed.
 -
 - Similar examples exist in
 -   Soar with Haskell, T. Schrijvers (Chapter 8)
 -
 -}

specification :: Integer -> Specification
specification x =
  anyOptionalOutput <>
  tillExit (
    readInput guess ints AssumeValid <>
    branch (currentValue guess .==. intLit x)
      exit
      (branch (currentValue guess .<. intLit x)
        (writeOutput [text "Guess again, but higher"])
        (writeOutput [text "Guess again, but lower"])
      )
  ) <>
  writeOutput [text "Correct, you found the number!"] <>
  writeOutput [wildcard <> resultOf (length' $ allValues guess) <> wildcard]
  where
    guess = intVar "guess"

main :: MonadTeletype io => Integer -> io ()
main x = do
  putStrLn "Guess my number!"
  n <- guess 0 x
  putStrLn $ "It took you " ++ show n ++ " guesses to find my number"

guess :: MonadTeletype io => Int -> Integer -> io Int
guess n x = do
  y <- readLn
  let n' = n+1
  case compare x y of
    LT -> do
      putStrLn "Guess again, but lower"
      guess n' x
    GT -> do
      putStrLn "Guess again, but higher"
      guess n' x
    EQ -> do
      putStrLn "Correct, you found the number!"
      pure n'
