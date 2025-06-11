{-# LANGUAGE TypeApplications #-}
module Simple.ReadNSum where

import Prelude hiding (print, putStrLn, readLn, getLine)
import Test.IOTasks

{- The program first reads a positive integer n from the console,
 - and then reads n integers one after the other and finally outputs
 - their sum.
 -
 - Similar examples exist in
 -   Programming in Haskell, G. Hutton, 2nd Edition (Chapter 10.10)
 -   Haskell, the craft of functional programming, S. Thompson, 3rd Edition (Chapter 8.4)
 -   Soar with Haskell, T. Schrijvers (Chapter 8)

 - Variants:
 -   * product instead of sum (or any other function on [Int], (Thompson has an example with sort))
 -   * with extra prompts, including which count of summand is to be input next
 -   * instead of reading in the number of additional numbers upfront,
       instead read until 0 is entered
 -}

basic :: Specification
basic =
  readInput n pos AssumeValid <>
  whileNot (currentValue n .==. length' (allValues x))
    (readInput x ints AssumeValid)
  <>
  writeOutput [wildcard <> resultOf (sum' (allValues x)) <> wildcard]
  where
    n = intVar "n"
    x = intVar "x"

prog :: MonadTeletype io => io ()
prog = do
  n <- readLn
  loop n 0
  where
    loop 0 s = print s
    loop n s = do
      x <- readLn
      loop (n-1) (s+x)

-- == Variants ==

product :: Specification
product =
  readInput n pos AssumeValid <>
  whileNot (currentValue n .==. length' (allValues x))
    (readInput x ints AssumeValid)
  <>
  writeOutput [wildcard <> resultOf (product' (allValues x)) <> wildcard]
  where
    n = intVar "n"
    x = intVar "x"

extraOutputs :: Specification
extraOutputs =
  anyOptionalOutput <>
  readInput n pos AssumeValid <>
  whileNot (currentValue n .==. length' (allValues x))
    ( writeOutput [wildcard <> resultOf (intLit 1 .+. length' (allValues x)) <> wildcard] <>
      readInput x ints AssumeValid
    )
  <>
  writeOutput [wildcard <> resultOf (product' (allValues x)) <> wildcard]
  where
    n = intVar "n"
    x = intVar "x"

stopOnZero :: Specification
stopOnZero =
  readInput n pos AssumeValid <>
  readInput x ints AssumeValid
    `repeatUntil` (currentValue n .==. intLit 0)
  <>
  writeOutput [wildcard <> resultOf (sum' (allValues x)) <> wildcard]
  where
    n = intVar "n"
    x = intVar "x"

pos :: ValueSet Integer
pos = greaterThan 0

