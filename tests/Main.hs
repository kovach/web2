-- TODO use a unit test framework
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Monad (tuples)
import Convert (runProgram)

import Control.Monad (unless)
import System.Exit (exitFailure)

data TestOutput = TO
  { tuple_count :: Int
  , steps_used :: Int
  , msgs_sent :: Int
  } deriving (Show, Eq, Ord)

type TestCase = (String, FilePath, FilePath, TestOutput)

-- TODO read expected output from text files instead
--      add function to generate these files
testCases :: [TestCase]
testCases =
  [ ("go-2x2-capture", "examples/go.arrow", "tests/go.graph",
      TO { tuple_count = 48
         , steps_used = 69
         , msgs_sent = 87
         } )
  , ("sieve_50", "examples/sieve.arrow", "tests/sieve.graph",
      TO { tuple_count = 194
         , steps_used = 273
         , msgs_sent = 207
         } )
  , ("factorial_6", "examples/factorial.arrow", "tests/factorial.graph",
      TO { tuple_count = 10
         , steps_used = 16
         , msgs_sent = 9
         } )
  , ("rule110_8x8_ui", "examples/110.arrow", "tests/110.graph",
      TO { tuple_count = 53
         , steps_used = 54
         , msgs_sent = 626
         } )
  ]


runTest :: TestCase -> IO Bool
runTest (label, rules, input, output) = do
  (_, _, _, result, outputs, _, gas, _) <- runProgram "" input rules
  let t = TO
          { tuple_count = length (fromGraph (tuples result))
          , steps_used = gas
          , msgs_sent = length outputs
          }
  if (t == output)
    then do
      putStrLn ("\nsuccess on case: " ++ label)
      return True
    else do
      putStrLn ("\nfailure on case: " ++ label ++ "\noutput:")
      print t
      putStrLn "expected:"
      print output
      return False

tests = and <$> mapM runTest testCases

main = do
  okay <- tests
  if okay then return () else exitFailure
