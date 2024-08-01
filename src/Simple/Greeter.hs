{-# LANGUAGE TypeApplications #-}
module Simple.Greeter where

import Prelude hiding (print, putStrLn, readLn, getLine)
import Test.IOTasks

{- Basic task:
 - Ask th user for their name, then greet them with a short
 - personalized message.
 -
 - Similar examples exist in
 -   Soar with Haskell, T. Schrijvers (Chapter 8)
 -   Haskell, the craft of functional programming, S. Thompson, 3rd Edition (Chapter 8.4)

 -
 - Variants:
 -   * simply repeating some user input back (multiple times)
 -}

basic :: Specification
basic =
  writeOutput [text "What is your name?"] <>
  readInput name str AssumeValid <>
  writeOutput [text "Hello, " <> resultOf (as @String $ currentValue name) <> text "!"]
  where
    name = stringVar "name"

main :: MonadTeletype io => io ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"
