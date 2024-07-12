module RaftLog where

import Control.Monad.State

import RaftTypes


type Log = [LogEntry]
type LogState a = State Log a

empty :: Log
empty = [LogEntry(-1) ""]

appendEntries :: Int -> Int -> [LogEntry] -> LogState Bool
appendEntries prevIndex prevTerm entries = do
  log <- get
  if prevIndex >= length log then
    return False
  else if term (log !! prevIndex) /= prevTerm then
    return False
  else do
    appendAtIndex (prevIndex + 1) entries
    return True

prevIndex :: LogState Int
prevIndex = do
  log <- get
  return $ length log - 1

prevTerm :: LogState Int
prevTerm = do
  log <- get
  let index = length log - 1
  return $ term (log !! index)

appendAtIndex :: Int -> Log -> LogState ()
appendAtIndex _ [] = return ()
appendAtIndex index (entry:entries) = do
  log <- get
  let (before, after) = splitAt index log
      log' = before ++ [entry] ++ dropWhile (\x -> term x == term entry) after
  put log'
  appendAtIndex (index + 1) entries

runLogState :: LogState a -> Log -> (a, Log)
runLogState = runState

-- | Tests
exampleEntries :: [LogEntry]
exampleEntries = [LogEntry 1 "x"]

main :: IO ()
main = do
  let (prevIndex', _) = runLogState prevIndex empty
  let (prevTerm', _) = runLogState prevTerm empty
  let (result, log') = runLogState (appendEntries prevIndex' prevTerm' [LogEntry 1 "x"]) empty
  putStrLn $ "Append result: " ++ show result
  putStrLn $ "Log after first append: " ++ show (drop 1 log')
  assert (drop 1 log' == [LogEntry 1 "x"]) "First append assertion failed"

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error msg
