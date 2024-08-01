{-# LANGUAGE TypeApplications #-}
module Games.Hangman where

import Prelude hiding (print, putStrLn, readLn, getLine)
import Test.IOTasks

{- A simple hangman game on number sequences
 - Similar to the examples found in
 -    Programming in Haskell, G. Hutton, 2nd Edition (Chapter 10.6)
 -    Haskell Programming from first principles, C: Allen, J. Moronuki (Chapter 13.9)
 -
 - Instead of guessing the complete sequence every time, a guess
 - is simply a number. The program then can respond to the number with
 - either "Correct!" or "Wrong!". If all numbers are guessed the program
 - outputs "You win!".
 - The program outputs the game state in between guesses.
 - (See limitedSpec below for a version with a limited number of guesses)
 -
 - Test as
 -   let secret = ... in taskCheck (hangmanProg secret) (hangmanSpec secret)
 -}

hangmanSpec :: [Integer] -> Specification
hangmanSpec word = tillExit (
     branch (winCond $ allValues guess) (writeOutput [text "You win!"] <> exit) mempty
  <> writeOutput [text "Game state:"  <> wildcard]
  <> readInput guess (unique digits) AssumeValid
  <> branch ((currentValue guess `isIn` listLit word) .&&. (currentValue guess `isNotIn` valuesBefore 1 guess))
    (writeOptionalOutput [text "Correct!"])
    (writeOptionalOutput [text "Wrong!"])
  )
  where
    winCond :: Term k [Integer] -> Term k Bool
    winCond g = foldr (\a b -> intLit a `isIn` g .&&. b) true word
    guess = intVar "guess"

digits :: ValueSet Integer
digits = (singleton 0 `union` greaterThan 0) `intersection` lessThan 10

hangmanProg :: MonadTeletype m => [Int] -> m ()
hangmanProg word = go [] where
  go guessed
    | all (`elem` guessed) word = putStrLn "You win!"
    | otherwise = do
        putStrLn $ "Game state:" ++ printWord word guessed
        x <- read <$> getLine
        if x `Prelude.elem` word Prelude.&& x `Prelude.notElem` guessed
          then do
            putStrLn "Correct!" -- this is optional
            go (x:guessed)
          else do
            putStrLn "Wrong!" -- this is optional
            go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed = Prelude.foldr (\x -> (++) (if x `Prelude.elem` guessed then show x ++ " " else "_ ")) "" xs

-- with limited number of guesses
limitedSpec :: Integer -> [Integer] -> Specification
limitedSpec maxGuesses word = tillExit (
     branch (winCond $ allValues guess) (writeOutput [text "You win!"] <> exit) mempty
  <> branch (length' @Integer (allValues guess) .==. intLit maxGuesses) (writeOutput [text "You loose!"] <> exit) mempty
  <> writeOutput [text "Game state:" <> wildcard <> resultOf (intLit maxGuesses .-. length' @Integer (allValues guess)) <> wildcard]
  <> readInput guess ints AssumeValid
  <> branch ((currentValue guess `isIn` listLit word) .&&. (currentValue guess `isNotIn` valuesBefore 1 guess))
    (writeOptionalOutput [text "Correct!"])
    (writeOptionalOutput [text "Wrong!"])
  )
  where
    winCond :: Term k [Integer] -> Term k Bool
    winCond g = foldr (\a b -> intLit a `isIn` g .&&. b) true word
    guess = intVar "guess"

limitedProg :: MonadTeletype m => Int -> [Int] -> m ()
limitedProg maxGuesses word = go maxGuesses [] where
  go n guessed
    | all (`elem` guessed) word = putStrLn "You win!"
    | n <= 0 = putStrLn "You loose!"
    | otherwise = do
        putStrLn $ "Game state:" ++ printWord word guessed
        putStrLn $ "Remaining guesses: " ++ show n
        x <- read <$> getLine
        if x `Prelude.elem` word Prelude.&& x `Prelude.notElem` guessed
          then do
            putStrLn "Correct!" -- this is optional
            go (n-1) (x:guessed)
          else do
            putStrLn "Wrong!" -- this is optional
            go (n-1) guessed
