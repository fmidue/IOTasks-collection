module Simple.Palindrome where

import Prelude hiding (print, putStr, getLine)
import Test.IOTasks

{- Read a line and output whether that line is a palindrome
 -
 - A similar example exists
 -   Haskell, the craft of functional programming, S. Thompson, 3rd Edition (Chapter 8.5)
 -
 - Variants: When using the 'withOpaque' template we can use any output function
 -           with type [Integer] -> Bool or [Integer] -> String by lifting it with
 -           'liftOpaque'.
 -}

-- has two paths (tests with palindromes and non palindromes as inputs)
simple :: Specification
simple =
  readInput x str AssumeValid <>
  writeOutput [text "is palindrome? "] <>
  branch (isPalindrome (currentValue x))
    (writeOutput [text "True"])
    (writeOutput [text "False"])
  where
    x = stringVar "x"

-- has only one path (not guaranteed to test palindromes)
withOpaque :: Specification
withOpaque =
  readInput x str AssumeValid <>
  writeOutput [text "is palindrome? " <> resultOf (liftOpaque (show,"show") (isPalindrome (currentValue x)))]
  where
    x = stringVar "x"

isPalindrome :: Term k String -> Term k Bool
isPalindrome x = x .==. reverse' x

prog :: MonadTeletype io => io ()
prog = do
  x <- getLine
  putStr "is palindrome? "
  print (reverse x == x)
