module RaftCore where

import Control.Concurrent
import Control.Monad.State

import RaftConfig
import RaftMessage

data Role =
  Leader | Candidate | Follower
  deriving (Show, Eq)

-- for now keep the state of Raft simple
data Raft = Raft
  { role    :: Role
  , nodeNum :: Node
  , peers   :: [Int] -- Node?
  }
  deriving (Show, Eq)

-- Would be better to use a TVar instead
-- TVar requires that there is always a item in the 'box'
newtype RaftState = RaftState (MVar Raft)

initial :: Node -> Raft
initial node = Raft
  { role    = Follower
  , nodeNum = node
  , peers   = getPeers node
  }

new :: Node -> IO RaftState
new node = do
  m <- newMVar $ initial node
  return $ RaftState m

updateFollowers :: Raft -> [Message]
updateFollowers Raft{nodeNum, peers} =
  [newMessage nodeNum peer | peer <- peers]
  where
    newMessage :: Node -> Node -> Message
    newMessage src dest =
      Message src dest 1 (AE (AppendEntries 0 0 [] 0))

handleMessage :: Message -> [Message]
handleMessage Message{source = src, dest = dest', payload = AE AppendEntries{entries = es}} =
  [Message dest' src 1 (AER(AppendEntriesResponse True (length es)))]
handleMessage Message{payload = AER AppendEntriesResponse{}} =
  []

becomeFollower :: RaftState -> IO Role
becomeFollower (RaftState m) = do
  raft <- takeMVar m
  putMVar m raft { role = Follower }
  return Follower

becomeLeader :: RaftState -> IO Role
becomeLeader (RaftState m) = do
  raft <- takeMVar m
  putMVar m raft { role = Leader }
  return Leader


--runRaftState :: RaftState a -> Raft -> IO (a, Raft)
--runRaftState = runStateT


