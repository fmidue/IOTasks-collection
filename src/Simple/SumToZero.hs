{-# LANGUAGE TypeApplications #-}
module Simple.SumToZero where

import Prelude hiding (print, putStr, putStrLn, readLn, getLine)
import Test.IOTasks
import Test.IOTasks.Constraints

{- Basic task:
 - Write a program that reads in two integers (negative integers too)
 - and prints out their sum. This behavior is repeated until the first
 - of the two numbers read is 0. The program then terminates (not
 - reading a second number), printing the count of additions
 - performed.
 -
 - You can add additional information to both the output of the
 - addition results as well as the final output. Furthermore you might
 - want to add additional outputs to indicate what the user has to do
 - next.
 -
 - Variant:
 -   * no negative numbers are allowed, when read number is negative read
 -     again, until the input is non-negative.
 -}

basic :: Specification
basic = specification (ints, AssumeValid) (ints, AssumeValid)

-- recommended settings stdArgs{ maxIterationUnfold = 10, maxNegatives = 2 }
noNegatives :: Specification
noNegatives = specification (nonNeg, UntilValid) (nonNeg, UntilValid)

{- A common error is to also abort on reading 0 for the second number.
 - From the specification alone no special test case is generated for this behavior,
 - therefore the counterexample is likely bigger than one would expect.
 - A second test against a narrower specification can make feedback more precise.
 -}
extraTest :: Specification
extraTest = specification (nonNeg, AssumeValid) (fromList [-2..2], AssumeValid)

extraTestNoNeg :: Specification
extraTestNoNeg = specification (nonNeg,UntilValid) (fromList [0..2], AssumeValid)

-- specification components
specification :: (ValueSet Integer, InputMode) -> (ValueSet Integer, InputMode) -> Specification
specification (valuesForX, modeForX) (valuesForY, modeForY) =
  tillExit (
    anyOptionalOutput <>
    readInput x valuesForX modeForX <>
    branch (currentValue x .==. intLit 0)
      exit
      ( anyOptionalOutput <>
        readInput y valuesForY modeForY <>
        writeOutput [wildcard <> resultOf (currentValue x .+. currentValue y)]
      )
  ) <>
  writeOutput [wildcard <> resultOf (length' $ allValues y) <> wildcard]
  where
    x, y :: Var Integer
    x = intVar "x"
    y = intVar "y"

nonNeg :: ValueSet Integer
nonNeg = greaterThan (-1)

-- solution for the basic variant
programBasic :: MonadTeletype io => io ()
programBasic = loop readLn 0

-- solution for nonNegatives
programNoNegatives :: MonadTeletype io => io ()
programNoNegatives = loop safeRead 0

loop :: MonadTeletype io => io Integer -> Integer -> io ()
loop whichRead n = do
  putStr "First number or 0 to exit: "
  x <- whichRead
  if x == 0
    then do
      putStrLn "Exiting program"
      putStr "The number of additions performed was: "
      print n
    else do
      putStr "Second number: "
      y <- whichRead
      putStr ("The sum of " ++ show x ++ " and " ++ show y ++ " is ")
      print (x + y)
      loop whichRead (n + 1)

safeRead :: MonadTeletype io => io Integer
safeRead = do
  x <- readLn
  if x < 0
    then do
      putStrLn "I don't like negative numbers at all."
      putStr "Please try again: "
      safeRead
    else
      return x
