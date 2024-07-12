{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RaftMessage where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Data.Aeson.Types (SumEncoding(..), defaultOptions, Options(..))


import RaftTypes
import RaftLog

type Source = Int
type Dest = Int
type PrevLogIndex = Int
type PrevLogTerm = Int
type LeaderCommit = Int

data Message = Message
  { source  :: Source
  , dest    :: Dest
  , term    :: Term
  , payload :: Payload
  } deriving (Show, Eq, Generic)

data AppendEntries = AppendEntries
  { prevLogIndex ::PrevLogIndex
  , prevLogTerm  :: PrevLogTerm
  , entries      :: [LogEntry]
  , leaderCommit :: LeaderCommit
  } deriving (Show, Eq, Generic)

data AppendEntriesResponse = AppendEntriesResponse
  { success    :: Bool
  , matchIndex :: Int
  } deriving (Show, Eq, Generic)

data Payload
  = AE AppendEntries
  | AER AppendEntriesResponse
  deriving (Show, Eq, Generic)

customOptions :: Options
customOptions = defaultOptions { sumEncoding = ObjectWithSingleField }

instance ToJSON LogEntry
instance FromJSON LogEntry

instance ToJSON AppendEntries
instance FromJSON AppendEntries

instance ToJSON AppendEntriesResponse
instance FromJSON AppendEntriesResponse

instance ToJSON Payload where
    toJSON = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON Payload where
    parseJSON = genericParseJSON customOptions

instance ToJSON Message where
    toJSON = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Message where
    parseJSON = genericParseJSON defaultOptions

-- Serialize to ByteString (JSON)
serialize :: ToJSON a => a -> BL.ByteString
serialize = encode

-- Deserialize from ByteString (JSON)
deserialize :: FromJSON a => BL.ByteString -> Maybe a
deserialize = decode

-- Example usage
main :: IO ()
main = do
    let msg1 = Message 0 1 1 (AE (AppendEntries 0 0 [] 0))
    let serializedMsg1 = serialize msg1
    putStrLn $ "Serialized Message: " ++ show serializedMsg1

    let deserializedMsg1 = deserialize serializedMsg1 :: Maybe Message
    putStrLn $ "Deserialized Message: " ++ show deserializedMsg1

    let msg2 = Message 0 1 1 (AER (AppendEntriesResponse True 1))
    let serializedMsg2 = serialize msg2
    putStrLn $ "Serialized Message: " ++ show serializedMsg2

    let deserializedMsg2 = deserialize serializedMsg2 :: Maybe Message
    putStrLn $ "Deserialized Message: " ++ show deserializedMsg2
