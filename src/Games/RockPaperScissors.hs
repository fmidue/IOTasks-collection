{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Games.RockPaperScissors where

import Prelude hiding (getChar, putStrLn, getLine, readLn)

import Data.Time
import GHC.IO.Unsafe (unsafePerformIO)
import Test.IOTasks
import Data.List (inits)
import Test.IOTasks.ValueMap
import Test.IOTasks.Var
import Test.IOTasks.OutputPattern

{- Taken with slight modification from
 - "Haskell, the craft of functional programming"
 -
 - The specification does not work for the hacky
 - randomStrategy that uses unsafePerformIO.
 -}

specification :: Strategy -> Specification
specification strategy =
  tillExit (
    readInput move (fromList [0,1,2,-1]) AssumeValid <>
    branch (currentValue move `isNotIn` listLit [0,1,2])
      exit
      (writeOutput [wildcard <> text "I play: " <> resultOf opponentMove <> text " you play: " <> resultOf yourMove])
  ) <>
  writeOutput [resultOf finalTournament]
  where
    move = intVar "m"
    yourMove :: Term 'PartiallyOpaque String
    yourMove = liftOpaque (show . convertToMove,"show move") $ currentValue move
    opponentMove :: Term 'PartiallyOpaque String
    opponentMove = liftOpaque (show . strategy . map convertToMove,"apply strategy") $ valuesBefore 1 move
    finalTournament :: Term 'PartiallyOpaque String
    finalTournament = liftOpaque (displayResults . reconstructTournament strategy . map convertToMove . tail,"show results") $ allValues move

reconstructTournament :: Strategy -> [Move] -> Tournament
reconstructTournament strategy (reverse -> moves) = (,moves) $ map strategy $ init $ inits $ moves

testP :: OutputPattern 'SpecificationP
testP = resultOf $ liftOpaque (id :: Integer -> Integer,"show move") $ currentValue move
-- testP = resultOf $ liftOpaque (show . convertToMove,"show move") $ currentValue move
  where move = intVar "move"

testM :: ValueMap
testM = insertValue (wrapValue @Integer 1) someMove $ emptyValueMap [someMove]
  where someMove = someVar $ intVar "move"

play :: MonadTeletype io => Strategy -> io ()
play strategy =
  playInteractive strategy ([],[])

-- replacement for getChar, so the specification can work in Integers
getMoveChar :: MonadTeletype io => io Char
getMoveChar = do
  x <- readLn
  pure $ case x of
    0 -> 'R'
    1 -> 'P'
    2 -> 'S'
    -1 -> 'q'

playInteractive :: MonadTeletype io => Strategy -> Tournament -> io ()
playInteractive s t@(mine,yours) = do
  -- ch <- getChar
  ch <- getMoveChar
  if ch `notElem` "rpsRPS"
    then showResults t
    else do
      let next = s yours
      let yourMove = convertMove ch
      putStrLn ("\nI play: " ++ show next ++ " you play: " ++ show yourMove)
      playInteractive s (next:mine, yourMove:yours)

showResults :: MonadTeletype io => Tournament -> io ()
showResults = putStrLn . displayResults

displayResults :: Tournament -> String
displayResults t =
  case compare (result t) 0 of
    GT -> "I won!"
    EQ -> "Draw!"
    LT -> "You won: well done!"

data Move = Rock | Paper | Scissors
  deriving (Show,Eq)

type Tournament = ([Move],[Move])

outcome :: Move -> Move -> Integer
outcome Rock Rock = 0
outcome Paper Paper = 0
outcome Scissors Scissors = 0
outcome Rock Scissors = 1
outcome Scissors Paper = 1
outcome Paper Rock = 1
outcome Rock Paper = -1
outcome Paper Scissors = -1
outcome Scissors Rock = -1

result :: Tournament -> Integer
result = sum . uncurry (zipWith outcome)

type Strategy = [Move] -> Move

rock, paper, scissors :: Strategy
rock _ = Rock
paper _ = Paper
scissors _ = Scissors

cycle :: Strategy
cycle moves =
  case length moves `rem` 3 of
    0 -> Rock
    1 -> Paper
    2 -> Scissors
    _ -> error "impossible"

randomStrategy :: Strategy
randomStrategy _ = convertToMove $ randInt 3

convertMove :: Char -> Move
convertMove 'r' = Rock
convertMove 'R' = Rock
convertMove 'p' = Paper
convertMove 'P' = Paper
convertMove 's' = Scissors
convertMove 'S' = Scissors
convertMove _ = error "invalid argument"

convertToMove :: Integer -> Move
convertToMove 0 = Rock
convertToMove 1 = Paper
convertToMove 2 = Scissors
convertToMove _ = error "invalid argument"

randomInt :: Integer -> IO Integer
randomInt n =
    do
      time <- getCurrentTime
      return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)

randInt :: Integer -> Integer
randInt = unsafePerformIO . randomInt
