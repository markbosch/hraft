module Main where

-- Standard
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Text.Printf

-- Raft
import RaftConfig
import RaftMessage
import RaftNet
import RaftCore

-- next:
-- - execute operation on the thread -> haskell is lazy

handleOutgoing net messages = do
  mapM_ (\msg -> send net (dest msg) (serialize msg)) messages

heartBeatTimer :: RaftState -> RaftNet -> IO ()
heartBeatTimer (RaftState m) net = forever $ do
  raft <- readMVar m
  if role raft == Leader
     then do
       let messages = updateFollowers raft
       handleOutgoing net messages
       return ()
     else return ()
  threadDelay 1000000  -- delayMs ?


run :: Node -> IO ()
run node = do
  net <- createRaftNet node
  start net
  forkIO $ receiver net
  raft <- new node
  forkIO $ heartBeatTimer raft net
  liftIO $ console net raft
  putStrLn "Final"
  where
    receiver net = forever $ do
      msg <- receive net
      printMsg (show msg)
      case deserialize msg of
        Just x -> do
          let outgoing = handleMessage x
          handleOutgoing net outgoing
        Nothing -> putStrLn "Ooops..."
    --receiver net = forever $ do
    --  msg <- receive net
    --  printMsg (show msg)
    printMsg :: String -> IO ()
    printMsg msg = putStrLn $ msg


console :: RaftNet -> RaftState -> IO ()
console net (RaftState m) = do
  forever $ do
    liftIO $ printf "Node %d > " $ node net
    cmd <- liftIO $ getLine
    case words cmd of
      ["heartbeat", dest] -> liftIO $ do
        raft <- readMVar m
        let messages = updateFollowers raft
        handleOutgoing net messages
        return ()
      ["leader"] -> do
        _ <- becomeLeader (RaftState m)
        print "Became Leader"
        return ()
      ["follower"] -> do
        _ <- becomeFollower (RaftState m)
        print "Became Follower"
        return ()
--      [dest, msg] -> send net (read dest) $ BL.fromStrict $ BS8.pack msg
--      [_] -> print "Wrong input"
  
main :: IO ()
main = do
  node <- getLine
  run $ read node
