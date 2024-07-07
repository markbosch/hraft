module RaftConfig where

import qualified Data.Map as Map
import Network.Socket

type Node = Int

servers :: [(Node, SockAddr)]
servers = [
  (0, SockAddrInet 15000 0x0100007f),
  (1, SockAddrInet 16000 0x0100007f),
  (2, SockAddrInet 17000 0x0100007f),
  (3, SockAddrInet 18000 0x0100007f),
  (4, SockAddrInet 19000 0x0100007f)
  ]

getServerAddress i = snd $ Map.elemAt i $ Map.fromList servers
