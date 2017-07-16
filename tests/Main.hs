-- TODO use a unit test framework
--      check for multiple deletion
--      check size of FactState
--      validate actual output
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Monad
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
         , msgs_sent = 85
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
  , ("anti_check", "tests/antipode.arrow", "tests/antipode.graph",
      TO { tuple_count = 9
         , steps_used = 6
         , msgs_sent = 11
         } )
  ]


runTest :: TestCase -> IO Bool
runTest (label, rules, input, output) = do
  (_, _, s) <- runProgram input rules
  let result = db s
      outputs = netOutput s
      steps = defaultGas - gas s
  let t = TO
          { tuple_count = length (fromGraph (tuples result))
          , steps_used = steps
          , msgs_sent = length outputs
          }
  putStrLn ("\ntest case: " ++ label)
  if (t == output)
    then do
      putStrLn ("success: " ++ label)
      return True
    else do
      putStrLn ("failure: " ++ label ++ "\noutput:")
      print t
      putStrLn "expected:"
      print output
      return False

tests = and <$> mapM runTest testCases

main = do
  okay <- tests
  if okay then return () else exitFailure
