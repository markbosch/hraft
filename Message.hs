module Message
  (sendMessage,
   receiveMessage
  )
  where

import Control.Exception (throwIO)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS
import Text.Printf

sendMessage :: Socket -> BS.ByteString -> IO ()
sendMessage sock msg = do
  sendAll sock size
  sendAll sock msg
  where
    -- | Make a 10-byte length field
    size :: BS.ByteString
    size = BS.pack . printf "%010d" $ BS.length msg

-- | Receive a message from a socket
receiveMessage :: Socket -> IO BS.ByteString
receiveMessage socket = do
  sizeStr <- receiveExactly socket 10
  let size = read (BS.unpack sizeStr) :: Int
  receiveExactly socket size

-- | Receive exactly `nbytes` from the socket.
receiveExactly :: Socket -> Int -> IO BS.ByteString
receiveExactly sock nbytes = loop nbytes []
  where
    loop 0 chunks = return $ BS.concat $ reverse chunks
    loop remaining chunks = do
      chunk <- recv sock remaining
      if BS.null chunk
        then throwIO $ userError "Incomplete message"
        else loop (remaining - BS.length chunk) (chunk : chunks)
