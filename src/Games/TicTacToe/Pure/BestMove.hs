{-# LANGUAGE StandaloneDeriving #-}
module Games.TicTacToe.Pure.BestMove where

import Data.List ( sortOn )
import Games.TicTacToe.Pure.Basics (Player, Pos)
import Games.TicTacToe.Pure.GameTree (GameTree(..), switch)

bestMove :: Player -> [ (Pos, GameTree) ] -> (Pos, Maybe Player)
bestMove player = bestMove' player . sortOn fst --  to avoid being impacted by student "rearrangement" of the continue list

bestMove' :: Player -> [ (Pos, GameTree) ] -> (Pos, Maybe Player)
bestMove' player continue =
  let
    outcomes = [ (outcome, move) | (move, tree) <- continue
                                 , let outcome = case tree of
                                                   Draw -> Nothing
                                                   Lost -> Just player
                                                   Continue xs -> snd (bestMove' (switch player) xs) ]
  in
    case lookup (Just player) outcomes of
      Just winning -> (winning, Just player)
      _ ->
        case lookup Nothing outcomes of
          Just drawing -> (drawing, Nothing)
          _ ->
            case outcomes of
              (Just p, losing):_ | p /= player -> (losing, Just p)
              _ ->
                error "We know that this branch is never reached!"

{- Examples:
 - ----------
 -
 -    1. If it is xPlayer's turn and the list passed is as follows:
 -
 -      [ ((A,Z), Continue [ ((C,X), Continue [ ((C,Y), Draw) ]),
 -                         , ((C,Y), Continue [ ((C,X), Lost) ]) ])
 -      , ((C,X), Lost)
 -      , ((C,Y), Continue [ ((A,Z), Lost)
 -                         , ((C,X), Continue [ ((A,Z), Draw) ]) ]) ]
 -
 -    then 'bestMove' should return the pair ((C,X), Just xPlayer).
 -    Because if xPlayer plays (C,X), then it would next be oPlayer's
 -    turn, but that player would have lost. Neither of the other two
 -    possible moves for xPlayer are equally good. Because if xPlayer
 -    plays (A,Z), oPlayer could respond with (C,X), after which
 -    xPlayer could only play (C,Y), resulting in a draw. If however
 -    xPlayer were to play (C,Y) instead, then oPlayer could respond
 -    with (A,Z), forcing a lose for xPlayer.
 -
 -    2. If it is xPlayer's turn and the list passed is as follows:
 -
 -       [ ((C,X), Continue [ ((C,Y), Draw) ])
 -       , ((C,Y), Continue [ ((C,X), Lost) ]) ]
 -
 -    then bestMove should return the pair ((C,X), Nothing), because
 -    in this situation a draw is the best possible outcome for
 -    xPlayer. (After all, the move (C,Y) would lead to oPlayer
 -    winning in the next turn.)
 -}
