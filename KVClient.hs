module KVClient
  where

import Network.Socket
import Control.Monad
import Message (sendMessage, receiveMessage)
import qualified Data.ByteString.Char8 as BS
import Text.Printf

main host port = do
  sock <- socket AF_INET Stream 0
  connect sock $ SockAddrInet port host
  loop sock
  where
    loop sock = forever $ do
      putStr "KV >"
      msg <- getLine
      if null msg
        then return ()
        else send sock msg
    send sock msg = do
      sendMessage sock $ BS.pack msg
      response <- receiveMessage sock
      putStrLn $ BS.unpack response

port :: PortNumber
port = 12345

host :: HostAddress
host = 0
