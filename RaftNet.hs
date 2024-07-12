module RaftNet where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Network.Socket
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Text.Printf

import RaftConfig
import Message

type Inbox = MVar BS.ByteString
type Outbox = MVar BS.ByteString

data RaftNet = RaftNet
  { node     :: Node
  , inbox    :: Inbox
  , outboxes :: Map.Map Node Outbox
  }

send :: RaftNet -> Node -> BS.ByteString -> IO ()
send net dest msg =
  case Map.lookup dest (outboxes net) of
    Just outbox -> putMVar outbox msg
    Nothing     -> error "Destination node not found."

receive :: RaftNet -> IO BS.ByteString
receive = takeMVar . inbox

start :: RaftNet -> IO ()
start net = do
  forkIO $ runAcceptor net
  forM_ (Map.toList $ outboxes net) $ \(dest, outbox) ->
    forkIO $ senderThread outbox dest
  print "Raft network started"

runAcceptor :: RaftNet -> IO ()
runAcceptor raft = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ getServerAddress (node raft)
  listen sock 5
  loop raft sock
  where
    loop :: RaftNet -> Socket -> IO ()
    loop raft sock = do
      (client, addr) <- accept sock
      _ <- async (receiveThread (client, addr) raft) -- todo: async or forkIO
      loop raft sock

-- | will put a message into the inbox
receiveThread :: (Socket, SockAddr) -> RaftNet -> IO ()
receiveThread (client, addr) raft = forever $ do
  msg <- receiveMessage client
  putMVar (inbox raft) msg

senderThread :: Outbox -> Node -> IO ()
senderThread outbox dest = forever $ do
  msg <- takeMVar outbox
  sock <- socket AF_INET Stream 0
  connect sock (getServerAddress dest)             -- todo: fix if is already connected
  sendMessage sock msg

-- | Hmmm...
createOutboxes :: IO (Map.Map Node (MVar BS.ByteString))
createOutboxes = do
  outboxMap <- Map.fromList <$> mapM (\(nodeNum, _) -> (nodeNum,) <$> newEmptyMVar) servers
  return outboxMap

-- | Would it be possible to 'inject' the createOutboxes as a function
createRaftNet :: Node -> IO RaftNet
createRaftNet node = do
  inbox <- newEmptyMVar
  outboxesMap <- createOutboxes
  return RaftNet
    { node     = node
    , inbox    = inbox
    , outboxes = outboxesMap
    }
