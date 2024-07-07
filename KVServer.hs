module KVServer
    ( run
    ) where

import Control.Monad (forever)
import Control.Monad.Trans.State (runStateT)
import Control.Concurrent (forkFinally, MVar, modifyMVar, newMVar)
import qualified Data.Map as Map
import Data.Map (Map)
import Network.Socket
import System.IO
    ( hSetBuffering,
      hGetLine,
      hPutStrLn,
      BufferMode(LineBuffering),
      IOMode(ReadWriteMode) )
import qualified Data.ByteString.Char8 as BS

import Message (sendMessage, receiveMessage)
import KVApp (handleMessage, initialState, Data)

type Port = Int

handleClient :: Socket -> MVar Data -> IO ()
handleClient sock stateMVar = forever $ do
    msg <- receiveMessage sock
    response <- modifyMVar stateMVar $ \state -> do
      (result, newState) <- runStateT (handleMessage $ BS.unpack msg) state
      return (newState, result)
    sendMessage sock $ BS.pack response

run :: HostAddress -> PortNumber -> IO ()
run host port = do
    stateMVar <- newMVar initialState
    sock <- socket AF_INET Stream 0
    
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet port host
    listen sock 2

    putStrLn $ "Listening on port " ++ show port
    serverLoop sock stateMVar

serverLoop :: Socket -> MVar Data -> IO ()
serverLoop sock stateMVar = do
    (conn, _) <- accept sock
    putStrLn "Accepted connection"
    forkFinally (handleClient conn stateMVar) (\_ -> close conn)
    serverLoop sock stateMVar
