{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as T (ByteString)
import Data.Aeson
import GHC.Generics

import System.Directory (listDirectory)
import Data.List (isSuffixOf)

import Types
import Monad
import Iterate

import BroadcastServer

import Debug.Trace

data Command = Reset | Connect
             | RawTuple {rawLabel :: Label, rawNodes :: [Node]}
             -- TODO: these should be a logical relation
             -- | Hover {ref :: Node} | UnHover {ref :: Node}
  deriving (Generic, Show)
deriving instance Generic Label
deriving instance Generic Id
deriving instance Generic Node
deriving instance Generic Polarity
instance ToJSON Label where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Node where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Polarity where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Command where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Label where
instance FromJSON Id where
instance FromJSON Node where
instance FromJSON Command where

jsCommands =
  [ "js/element"
  , "js/attr"
  , "js/edit"
  , "js/text"
  , "refresh-code-mirror"
  , "child"
  , "background-color"
  , "js/style"
  , "class"
  ]

decodeCommand = decode

convert :: Msg -> Maybe (Polarity, Label, [Node], Node, Bool)
convert (MT p t) | not (fix (label t) `elem` jsCommands) = Nothing
  where
    fix (LA s _) = L s
convert (MT p T{..}) = Just (p, label, nodes, NNode (Id tid), fix tval)
  where
    fix (Truth t) = t
    fix NoVal = True
    -- TODO!
    fix (TVNode n) = True

-- TODO remove arity tags?
encodeEvents :: [Msg] -> T.ByteString
encodeEvents = encode . mapMaybe convert

data State = State PS SystemState InterpreterState

type Init = (PS, DB, [Msg])

-- returns result of removing suffix, if it is a suffix
msuffix :: String -> String -> Maybe String
msuffix suf s = fix (reverse suf) (reverse s)
  where
    fix [] s = Just $ reverse s
    fix (a:as) (b:bs) | a == b = fix as bs
    fix _ _ = Nothing

scriptSuffix = ".arrow"
dataSuffix = ".stuff"

loadStuff dir str = do
    f <- readFile (dir ++ "/" ++ str ++ dataSuffix)
    let ls = lines f
    return $ do
      fid <- freshNode
      ls <- mapM (uncurry $ toLine fid) $ zip [0..] ls
      eof <- eof fid (length ls)
      fmsg <- fileMsg fid
      return $ fmsg : eof : ls
  where
    toLine f i l =
      MT Positive <$> packTuple (LA "io/line" 3, [f, NInt i, NString l]) (Extern [])
    eof f i =
      MT Positive <$> packTuple (LA "io/eof" 2, [f, NInt i]) (Extern [])
    fileMsg f =
      MT Positive <$> packTuple (LA "io/file" 1, [f]) (Extern [])

loadDirectory dir = do
  dirfiles <- listDirectory dir
  let scripts = mapMaybe (msuffix scriptSuffix) dirfiles
      resources = mapMaybe (msuffix dataSuffix) dirfiles
      files = map (\(i, s) -> (show i, dir ++ "/" ++ s ++ scriptSuffix)) (zip [1..] scripts)
  fileMsgsM <- sequence <$> mapM (loadStuff dir) resources
  strs <- mapM (readFile . snd) files
  let fix s = do
        n1 <- freshNode
        m1 <- packTuple (LA "make-app" 2, [n1, NString s]) (Extern [])
        return $ CMsg (MT Positive m1)
  return $ runStack emptySS emptyIS $ do
    ps <- initMetaPS (zip (map fst files) strs)
    ms <- lift $ mapM (fix . fst) files
    fileMsgs <- lift $ concat <$> fileMsgsM
    -- register rulesets
    (output1, ps1) <- solve ms ps
    worker <- gets worker_id
    (output2, ps2) <- solve (map (CActor worker) $ fileMsgs) ps1
    return (output2++output1, ps2)

makeDB1 = do
  let files = [ ("refl", "ui/components/refl.arrow")
              , ("button", "ui/components/button.arrow")
              -- ? TODO not using this:
              , ("rules",  "ui/components/rule-set.arrow") ]
  strs <- mapM (readFile . snd) files
  let fix s = do
        n1 <- freshNode
        m1 <- packTuple (LA "make-app" 2, [n1, NString s]) (Extern [])
        return $ CMsg (MT Positive m1)
  return $ runStack emptySS emptyIS $ do
    ps <- initMetaPS (zip (map fst files) strs)
    ms <- lift $ mapM (fix . fst) files
    -- register rulesets
    (output1, ps1) <- solve ms ps
    return (output1, ps1)

makeDB2 = do
  let files = [ ("ui", "ui/jelly/io.arrow")
              , ("level", "ui/jelly/level.arrow")
              , ("logic", "ui/jelly/logic.arrow")
              ]
  strs <- mapM (readFile . snd) files
  let fix s = do
        n1 <- freshNode
        m1 <- packTuple (LA "make-app" 2, [n1, NString s]) (Extern [])
        return $ CMsg (MT Positive m1)
  return $ runStack emptySS emptyIS $ do
    ps <- initMetaPS (zip (map fst files) strs)
    ms <- lift $ mapM (fix . fst) files
    -- register rulesets
    (output1, ps1) <- solve ms ps
    return (output1, ps1)

makeDB4 = loadDirectory "ui/slides"

noDebug = False

handler connId msg s0@(State ps ss is) =
  case decodeCommand msg of
    Just Reset -> do
      putStrLn "reset"
      (((msgs, ps'), ss'), is') <- makeDB4
      -- TODO only send relevant tuples
      --let (_, outputEvents) = step2 msgs emptyFS
      putStrLn $ "init: " ++ unlines (map ppMsg msgs)
      return (Just (encodeEvents msgs), State ps' ss' is')
    Just Connect -> do
      putStrLn "not implemented"
      return (Nothing, s0)
    Just RawTuple{rawLabel, rawNodes} -> do
      putStrLn "parsed event"
      let
        (((msgs, ps'), ss'), is') = runStack ss is $ do
          -- reset gas limit
          lift $ setGas 500
          -- TODO mark tuple with connection id
          let ns = rawNodes
              l = LA (lstring rawLabel) (length ns)
          t <- lift $ packTuple (l, ns) (Extern [])
          worker <- gets worker_id
          let msg = (CActor worker (MT Positive t))
          solve [msg] ps

      unless noDebug $ do
        putStrLn "new"
        mapM_ (putStrLn . ppMsg) msgs
      putStrLn "done"
      return (Just (encodeEvents msgs), State ps' ss' is')
    Nothing -> do
      putStrLn "decode failed"
      return (Nothing, s0)

main = do
  putStrLn "server starting"
  runServer (State emptyPS emptySS emptyIS) handler
