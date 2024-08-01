module Games.TicTacToe.Pure.GameTree where

import Games.TicTacToe.Pure.Basics
  (Row(..), Column(..), Pos, Player, Board,
  xPlayer, oPlayer, initialBoard,
  possibleMoves, makeMove, endPosition)

switch :: Player -> Player
switch player | player == xPlayer = oPlayer
              | otherwise         = xPlayer

data GameTree = Lost | Draw | Continue [ (Pos, GameTree) ]  deriving Show

outlook :: Player -> Board -> GameTree
outlook player board =
  case endPosition board of
    Just Nothing -> Draw
    Just (Just _) -> Lost
    Nothing ->
      let freePos = possibleMoves board
      in Continue [ (position, outlook (switch player) newBoard)
                  | position <- freePos
                  , let newBoard = makeMove player position board
                  ]

{- Here 'Lost' and 'Draw' express the corresponding outcome for the
 - given player in the given situation. If neither of these cases has
 - occurred yet, 'Continue' expresses a non-empty list of all pairs of
 - currently possible positions for the player to put his mark on and
 - the descriptions of their corresponding further developments, for
 - the opposing player whose turn it will then be.
 -
 - Example:
 - ---------
 -
 -   If the current board is as follows:
 -
 -       X Y Z
 -     A:X O .
 -     B:X X O
 -     C:. . O
 -
 -   and it is xPlayer's turn, then a correct value of
 -   'outlook xPlayer exampleBoard' is:
 -
 -     Continue [ ((A,Z), Continue [ ((C,X), Continue [ ((C,Y), Draw) ])
 -                                 , ((C,Y), Continue [ ((C,X), Lost) ]) ])
 -              , ((C,X), Lost)
 -              , ((C,Y), Continue [ ((A,Z), Lost)
 -                                 , ((C,X), Continue [ ((A,Z), Draw) ]) ]) ]
 -
 -   Because when xPlayer's move is (C,X), then the opposing player
 -   has lost. However when xPlayer's move is (C,Y), then the
 -   following oPlayer's move can be (A,Z) and xPlayer will have lost,
 -   or oPlayer moves (C,X) instead and leaves xPlayer no other choice
 -   than to move (A,Z), which leads to a draw. And if xPlayer in the
 -   original situation marks (A,Z), then possible moves for oPlayer
 -   are either (C,X) or (C,Y) with their corresponding continuations.
 -}

-- Here is the game board used above, for testing purposes:

exampleBoard :: Board
exampleBoard = initialBoard
               `andThen` makeMove xPlayer (B,Y)
               `andThen` makeMove oPlayer (A,Y)
               `andThen` makeMove xPlayer (B,X)
               `andThen` makeMove oPlayer (B,Z)
               `andThen` makeMove xPlayer (A,X)
               `andThen` makeMove oPlayer (C,Z)
  where andThen = flip ($)
