{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Games.TicTacToe.Interactive.AgainstCPU where

import Prelude hiding (print, putStrLn, putStr, readLn, getLine)
import Test.IOTasks
import Test.IOTasks.IOrep (stdout, BufferMode (NoBuffering))

import Games.TicTacToe.Pure.Basics ((!),initialBoard,makeMove,oPlayer,xPlayer,Board,Player,Pos,Column(..),Row(..))
import Games.TicTacToe.Pure.GameTree (GameTree(..), switch, outlook)
import Games.TicTacToe.Pure.BestMove (bestMove)

import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Data.List (sort)

-- == TicTacToe against a CPU opponent ==

--   X   Y   Z
-- A 8 | 1 | 6
--  ---|---|--
-- B 3 | 5 | 7
--  ---|---|--
-- C 4 | 9 | 2

tictactoeCPUSpec :: Specification
tictactoeCPUSpec = specification $ convert xPlayer $ outlook xPlayer initialBoard

{- The specification has exactly one possible input sequence per path.
 - Therefore maxSuccessPerPath > 1 does not increase test coverage
 -}
specification :: MGameTree -> Specification
specification MDraw = writeOutput [wildcard <> text "draw" <> wildcard]
specification LossPlayer = writeOutput [wildcard <> text "lost" <> wildcard]
specification WinPlayer = writeOutput [wildcard <> text "win" <> wildcard]
specification (CPUMove p t) = writeOutput [wildcard <> text (show p) <> wildcard] <> specification t
specification (PlayerChoice xs) =
  anyOptionalOutput <>
  readInput move (fromList $ map (posToNumber . fst) xs) AssumeValid <>
  branchOnInput xs
  where
    branchOnInput :: [(Pos, MGameTree)] -> Specification
    branchOnInput [] = writeOutput [text "invalid move"]
    branchOnInput ((p,t):ps) = branch (currentValue move .==. intLit (posToNumber p)) (specification t) (branchOnInput ps)

move :: Var Integer
move = intVar "move"

type BestMove = Pos
data MGameTree = PlayerChoice [(Pos,MGameTree)] | CPUMove BestMove MGameTree | WinPlayer | LossPlayer | MDraw
  deriving (Eq,Show)

convert :: Player -> GameTree -> MGameTree
convert _ Draw = MDraw
convert True Lost = LossPlayer
convert False Lost = WinPlayer -- will never happen
convert True (Continue xs) = PlayerChoice $ map (second $ convert False) xs
convert False (Continue xs) =
  let (pos,_) = bestMove False xs
  in CPUMove pos $ convert True $ fromJust $ lookup pos xs

posToNumber :: Pos -> Integer
posToNumber (A,X) = 8
posToNumber (A,Y) = 1
posToNumber (A,Z) = 6
posToNumber (B,X) = 3
posToNumber (B,Y) = 5
posToNumber (B,Z) = 7
posToNumber (C,X) = 4
posToNumber (C,Y) = 9
posToNumber (C,Z) = 2

numberToPos :: Integer -> Pos
numberToPos 8 = (A,X)
numberToPos 1 = (A,Y)
numberToPos 6 = (A,Z)
numberToPos 3 = (B,X)
numberToPos 5 = (B,Y)
numberToPos 7 = (B,Z)
numberToPos 4 = (C,X)
numberToPos 9 = (C,Y)
numberToPos 2 = (C,Z)
numberToPos _ = error "invalid Pos"

{- Replacement for reading in Pos by string input.
 - Enables precise input sets in the specification
 - as string inputs currently do not support subsets.
 -}
readPos :: MonadTeletype io => io Pos
readPos = numberToPos <$> readLn

tictactoeCPU :: MonadTeletype io => io ()
tictactoeCPU = do
  hSetBuffering stdout NoBuffering
  let player = xPlayer
  loop (player, outlook player initialBoard) initialBoard

loop :: MonadTeletype io => (Player, GameTree) -> Board -> io ()
loop (player, Lost)          board | player == xPlayer
                                   = putStrLn $ display board ++ "\nPlayer has lost."
loop (player, Draw)          board = putStrLn $ display board ++ (if player == xPlayer
                                                                  then "\nPlayer"
                                                                  else "\nOpponent")
                                                              ++ "'s turn, draw."
loop (player, Continue cont) board =
  do putStrLn $ display board
     putStrLn ""
     putStr $ if player == xPlayer then "Player's turn (in format '(A,X)' or '(b,y)'): " else "Opponent's turn. "
     move <- if player == xPlayer
             then do input <- readPos -- getLine
                     putStrLn ""
                     return input
                     --  return (read (map toUpper input))
             else do let (best, outcome) = bestMove oPlayer cont
                     putStrLn $ "I'll do " ++ show best ++ " now, and I know that the "
                                           ++ "best outcome I can enforce is to "
                                           ++ case outcome of
                                                Just thePlayer | thePlayer == oPlayer -> "win."
                                                Nothing -> "draw even."
                     putStrLn ""
                     return best
     let Just tree = lookup move cont
     loop (switch player, tree) (makeMove player move board)

display :: Board -> String
display board = unlines (map row [A .. ])
 where row :: Row -> String
       row r = concat [ maybe "." (take 1 . show) (board ! (r,c)) | c <- [X .. ]]
