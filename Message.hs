module Message
  (sendMessage,
   receiveMessage
  )
  where

import Control.Exception (throwIO)
import Data.Int(Int64)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Text.Printf

sendMessage :: Socket -> BL.ByteString -> IO ()
sendMessage sock msg = do
  sendAll sock size
  sendAll sock msg
  where
    -- | Make a 10-byte length field
    size :: BL.ByteString
    size = BL.fromStrict . BS8.pack . printf "%010d" $ BL.length msg

-- | Receive a message from a socket
receiveMessage :: Socket -> IO BL.ByteString
receiveMessage socket = do
  sizeStr <- receiveExactly socket 10
  let size = read (BS8.unpack (BL.toStrict sizeStr)) :: Int64
  receiveExactly socket size

-- | Receive exactly `nbytes` from the socket.
receiveExactly :: Socket -> Int64 -> IO BL.ByteString
receiveExactly sock nbytes = loop nbytes []
  where
    loop 0 chunks = return $ BL.concat $ reverse chunks
    loop remaining chunks = do
      chunk <- recv sock remaining
      if BL.null chunk
        then throwIO $ userError "Incomplete message"
        else loop (remaining - BL.length chunk) (chunk : chunks)
