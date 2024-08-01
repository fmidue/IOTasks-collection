module Games.TicTacToe.Pure.Basics where

import Data.Maybe ( isJust, isNothing )
import Data.List ( nub )

data Row    = A | B | C deriving (Show, Read, Eq, Ord, Enum, Bounded)
data Column = X | Y | Z deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Pos = (Row, Column)

type Player = Bool

newtype Board = Board [[Maybe Player]]

xPlayer :: Player
xPlayer = True

oPlayer :: Player
oPlayer = False

-- representation of the empty game board:
initialBoard :: Board
initialBoard = Board $ replicate 3 $ replicate 3 Nothing

-- lookup of a position in a given board:
--   board ! pos = Nothing      ==>  the position is still empty
--   board ! pos = Just player  ==>  the position was already taken
--                                   by the returned player
infixl 9 !
(!) :: Board -> Pos -> Maybe Player
(!) (Board board) (row, column) = board !! fromEnum row !! fromEnum column

-- free positions on a given board (no position returned twice):
possibleMoves :: Board -> [Pos]
possibleMoves board = filter (isNothing . (board !)) moves

-- updating the game board with a move by one of the two players,
makeMove :: Player -> Pos -> Board -> Board
makeMove player move board = Board [ [ if (row, column) == move
                                       then Just player
                                       else board ! (row, column) | column <- elems ] | row <- elems ]

-- testing whether the game is finished, and with which result:
--   endPosition board = Nothing       ==>  game can still be continued
--   endPosition board = Just Nothing  ==>  game is finished, with a draw
--   endPosition board = Just (Just player), where either player == xPlayer
--                                                 or     player == oPlayer
--                                     ==>  game is finished, and the
--                                          returned player has won
endPosition :: Board -> Maybe (Maybe Player)
endPosition board =  case nub [ player |  (m1,m2,m3) <- toTest,
                                          let player = board ! m1, isJust player,
                                          player == board ! m2,
                                          player == board ! m3]
                     of
                       []       -> if null (possibleMoves board) then Just Nothing else Nothing
                       [player] -> Just player
                       _:_:_    -> error "IMPOSSIBLE!"

toTest :: [((Row, Column), (Row, Column), (Row, Column))]
toTest = [ ((A,X),(A,Y),(A,Z)),
           ((A,X),(B,X),(C,X)),
           ((A,X),(B,Y),(C,Z)),
           ((A,Y),(B,Y),(C,Y)),
           ((A,Z),(B,Y),(C,X)),
           ((A,Z),(B,Z),(C,Z)),
           ((B,X),(B,Y),(B,Z)),
           ((C,X),(C,Y),(C,Z)) ]

elems :: (Bounded a, Enum a) => [a]
elems = [minBound .. maxBound]

moves :: [Pos]
moves = [ (row,column) | row <- elems, column <- elems ]
