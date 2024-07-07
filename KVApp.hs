module KVApp
    ( Data
    , initialState
    , handleMessage
    ) where

import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Map (Map)
import System.Posix (TerminalMode(KeyboardInterrupts))

type Key = String
type Value = String
type Data = Map Key Value
type KVStore = StateT Data IO

initialState :: Data
initialState = Map.empty

handleMessage :: String -> KVStore String
handleMessage msg = case words msg of
            ["set", key, value] -> do
                setValue key value
                return "Ok"
            ["get", key] -> do
                result <- getValue key
                case result of
                    Just value -> return value
                    Nothing    -> return "Not found"
            ["delete", key] -> do
                deleteValue key
                return "Ok"
            _ -> return "Unknown command"

setValue :: Key -> Value -> KVStore ()
setValue key value = do
    currentState <- get
    let newState = Map.insert key value currentState
    put newState

getValue :: Key -> KVStore (Maybe Value)
getValue key = Map.lookup key <$> get

deleteValue :: Key -> KVStore ()
deleteValue key = do
    currentState <- get
    let newState = Map.delete key currentState
    put newState
