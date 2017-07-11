{-# LANGUAGE OverloadedStrings #-}
module BroadcastServer (runServer, Handler) where

import Data.Monoid (mappend)
import Control.Monad (forever, unless)
import Control.Exception (finally)

import Data.ByteString.Lazy (ByteString)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy (ByteString)

import qualified Network.WebSockets as WS
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as WaiStatic

noDebug = False

type Id = Int
type Client = (Id, WS.Connection)
data SS a = SS
  { count :: Int
  , clients :: [Client]
  , world :: a }

initSS a = SS { count = 0, clients = [], world = a }

type Handler a = Id -> Data -> a -> IO (Maybe Data, a)
type Data = ByteString

broadcast :: Id -> Data -> [Client] -> IO ()
broadcast id msg clients = do
  mapM_ send clients
  where
    send (id', conn) = WS.sendTextData conn msg

-- TODO delete?
--sendData :: Id -> Data -> MVar (SS a) -> IO ()
--sendData id text state = do
--  SS {clients = cs} <- readMVar state
--  let Just conn = lookup id cs
--  WS.sendTextData conn text

talk :: Id -> WS.Connection -> MVar (SS t) -> Handler t -> IO ()
talk id conn state handler = forever $ do
  msg <- WS.receiveData conn
  unless noDebug $ T.putStrLn $ "got: " `mappend` (T.toStrict $ T.decodeUtf8 $ msg)
  SS {clients = cs, world = a} <- readMVar state
  (m, a') <- handler id msg a
  modifyMVar_ state $ \s -> return $ s {world = a'}
  maybe (return ()) (\m -> broadcast id m cs) m

application :: MVar (SS a) -> Handler a -> WS.PendingConnection -> IO ()
application state handler pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  SS {world = w, count = count} <- readMVar state
  let id = count + 1
  unless noDebug $ putStrLn $ "user #" ++ show id ++ " connected"
  modifyMVar_ state $ \s -> return s {count = id, clients = (id, conn) : clients s}
  -- Main loop
  finally (talk id conn state handler) (disconnect id state)

staticContent :: Network.Wai.Application
staticContent = WaiStatic.staticApp $ WaiStatic.defaultWebAppSettings "./static"

disconnect id state = do
  unless noDebug $ putStrLn $ "user #" ++ show id ++ " disconnected"
  modifyMVar_ state $ \s -> return $ removeClient id s

removeClient id s = s { clients = filter (not . (== id) . fst) (clients s) }

-- Entry point --
runServer a fn = do
  state <- newMVar (initSS a)
  Warp.run 8080 (WaiWS.websocketsOr WS.defaultConnectionOptions
                                    (application state fn)
                                    staticContent)
