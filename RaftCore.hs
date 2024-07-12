module RaftCore where

import RaftConfig
import RaftMessage

-- for now just a single node
updateFollowers :: Node -> Node -> [Message]
updateFollowers src dest = [Message src dest 1 (AE (AppendEntries 0 0 [] 0))]

handleMessage :: Message -> [Message]
handleMessage Message{source = src, dest = dest', payload = AE AppendEntries{entries = es}} =
  [Message dest' src 1 (AER(AppendEntriesResponse True (length es)))]
handleMessage Message{payload = AER AppendEntriesResponse{}} =
  []
