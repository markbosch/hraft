module Main where

-- Standard
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Text.Printf

-- Raft
import RaftConfig
import RaftMessage
import RaftNet
import RaftCore


handleOutgoing net messages = do
  mapM_ (\msg -> send net (dest msg) (serialize msg)) messages

run :: Node -> IO ()
run node = do
  net <- createRaftNet node
  start net
  console net

console :: RaftNet -> IO ()
console net = do
  forkIO $ receiver net
  forever $ do
    printf "Node %d > " (node net)
    cmd <- getLine
    case words cmd of
      ["heartbeat", dest] -> do
        let messages = updateFollowers (node net) (read dest)
        handleOutgoing net messages
--      [dest, msg] -> send net (read dest) $ BL.fromStrict $ BS8.pack msg
--      [_] -> print "Wrong input"
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

main :: IO ()
main = do
  node <- getLine
  run $ read node
