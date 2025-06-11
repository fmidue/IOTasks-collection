{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Games.TicTacToe.Interactive.TwoPlayers where

import Prelude hiding (print, putStrLn, putStr, readLn, getLine)
import Test.IOTasks
import Test.IOTasks.IOrep (stdout, BufferMode (NoBuffering))

import Games.TicTacToe.Pure.Basics ((!),initialBoard,makeMove,oPlayer,xPlayer,Board,Player,Pos,Column(..),Row(..))
import Games.TicTacToe.Pure.GameTree (GameTree(..), switch, outlook)
import Games.TicTacToe.Pure.BestMove (bestMove)

import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Data.List (sort)

-- == A "TicTacToe" game for two human players ==

{-
 - The game actually requires both players to pick unique numbers from 1-9 instead classical TicTacToe moves.
 - The first player to pick three number that sum up to 15 wins.
 -
 - This corresponds to labeling the TicTacToe board like so:
 -   X   Y   Z
 - A 8 | 1 | 6
 -  ---|---|--
 - B 3 | 5 | 7
 -  ---|---|--
 - C 4 | 9 | 2
 -
 - (The same embedding of game moves from Games.TicTacToe.Interactive.AgainstCPU is also possible here.)
 -}

-- does not need more than 3 iteration unfolds
tictactwoSpec :: Specification
tictactwoSpec =
  legalMoveX <>
  legalMoveO <>
  legalMoveX <>
  legalMoveO <>
  tillExit ( -- nobody can win before making their 3rd move
    legalMoveX <>
      branch (winCond x) (writeOutput [text "X wins"] <> exit)
        (branch draw (writeOutput [text "draw"] <> exit) -- draw can only happen after X moved
          (legalMoveO <> branch (winCond o) (writeOutput [text "O wins"] <> exit)
            nop)
          )
  )
  where
    x = intVar "X"
    o = intVar "O"
    wins = [ (x,y,z) | x <- [1..9], y <- [x+1..9], z <- [y+1..9], x+y+z == 15]
    winCond p = foldr (((.||.)) . (\(x,y,z) -> (intLit x `isIn` allValues p) .&&. (intLit y `isIn` allValues p) .&&. (intLit z `isIn` allValues p))) false wins
    draw = length' (allValues $ merge [x,o]) .==. intLit 9

    legalMoveX =
      writeOutput [text "X to move:"] <> readInput x (unique $ fromList [1..9] `notInVar` o) UntilValid
    legalMoveO =
      writeOutput [text "O to move:"] <> readInput o (unique $ fromList [1..9] `notInVar` x) UntilValid

tictactwo :: forall io. MonadTeletype io => io ()
tictactwo = play [] []
  where
    play :: [Int] -> [Int] -> io ()
    play xs os = do
      xs' <- readMove "X" (xs,os)
      if win xs' then putStrLn "X wins"
        else if sort (xs'++os) == [1..9] then putStrLn "draw"
          else do
            os' <- readMove "O" (os,xs')
            if win os' then putStrLn "O wins"
              else play xs' os'
    win ms = not $ null [(x, y, z) | x <- ms, y <- ms, x /= y, z <- ms, y /= z, x /= z, x + y + z == 15]
    readMove :: String -> ([Int],[Int]) -> io [Int]
    readMove p (self,other) = do
      putStrLn $ p ++ " to move:"
      m <- readLn
      if m `elem` [1..9] && m `notElem` self++other then pure (m:self) else readMove p (self,other)
