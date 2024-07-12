{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RaftTypes where

import GHC.Generics (Generic)

type Term = Int

data LogEntry = LogEntry
  { term    :: Term
  , command :: String
  } deriving (Show, Eq, Generic)

