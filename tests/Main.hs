-- TODO use a unit test framework
--      check for multiple deletion
--      validate actual output
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Types
import Monad
import Convert (runProgram)

data TestOutput = TO
  { event_count :: Int
  , tuple_count :: Int
  , steps_used :: Int
  , msgs_sent :: Int
  } deriving (Show, Eq, Ord)

type TestCase = (String, FilePath, FilePath, TestOutput)

-- TODO read expected output from text files instead
--      add function to generate these files
--
-- TODO remove msgs_sent field?
testCases :: [TestCase]
testCases =
  [ ("go-2x2-capture", "examples/go.arrow", "tests/go.graph",
      TO { event_count = 39
         , tuple_count = 49
         , steps_used = 87
         , msgs_sent = 91
         } )
  , ("sieve_50", "examples/sieve.arrow", "tests/sieve.graph",
      TO { event_count = 255
         , tuple_count = 256
         , steps_used = 230
         , msgs_sent = 303
         } )
  , ("factorial_6", "examples/factorial.arrow", "tests/factorial.graph",
      TO { event_count = 9
         , tuple_count = 9
         , steps_used = 16
         , msgs_sent = 8
         } )
  , ("rule110_8x8_ui", "examples/110.arrow", "tests/110.graph",
      TO { event_count = 43
         , tuple_count = 619
         , steps_used = 71
         , msgs_sent = 616
         } )
  , ("anti_check", "tests/antipode.arrow", "tests/antipode.graph",
      TO { event_count = 4
         , tuple_count = 8
         , steps_used = 10
         , msgs_sent = 7
         } )
  ]


runTest :: TestCase -> IO Bool
runTest (label, rules, input, output) = do
  (_, outputs, _, s) <- runProgram input rules
  let result = db s
      --outputs = netOutput s
      steps = defaultGas - gas s
      graph = fromGraph (tuples result)
      eventTuples = filter isEventTuple graph
      positiveTuples = filter isPositive graph
  let t = TO
          { tuple_count = length positiveTuples
          , event_count = length eventTuples
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
      putStrLn $ "assertion count: " ++ show (length positiveTuples)
      return False

tests = and <$> mapM runTest testCases

main = do
  okay <- tests
  if okay then return () else exitFailure
