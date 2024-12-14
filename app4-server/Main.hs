{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, put)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Web.Scotty
import qualified Lib3
import qualified Lib2
import Control.Concurrent (Chan, readChan, writeChan, newChan, forkIO)
import GHC.Conc (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Data.String.Conversions (cs)
import System.IO.Silently (capture)

parseCommandWithState :: String -> ExceptT String (StateT String IO) (String, Lib3.Command)
parseCommandWithState input = do
    lift $ State.put input 
    (output, resultIO) <- liftIO $ System.IO.Silently.capture $ do
        case Lib3.parseCommand input of
            Right (cmd, "") -> return (Right cmd)
            Right _ -> return (Left "Parse Error: not whole string has been consumed")
            Left err -> return (Left ("Parse Error: " ++ err))
    case resultIO of
        Right cmd -> return (output, cmd)
        Left err -> throwE $ output ++ err

main :: IO ()
main = do
    state <- newTVarIO Lib2.emptyState
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    _ <- forkIO $ Lib3.storageOpLoop chan  
    scotty 3000 $ do
        post "/upload" $ do
            b <- body
            let input = cs b
            let parser = parseCommandWithState input
            result <- liftIO $ runStateT (runExceptT parser) ""  
            case result of
                (Right (logs, cmd), _) -> do
                    liftIO $ putStrLn logs 
                    case cmd of
                        Lib3.SaveCommand -> do
                            result <- liftIO $ Lib3.stateTransition state Lib3.SaveCommand chan
                            respond result
                        Lib3.LoadCommand -> do
                            result <- liftIO $ Lib3.stateTransition state Lib3.LoadCommand chan
                            respond result
                        Lib3.StatementCommand b -> do
                            result <- liftIO $ Lib3.stateTransition state (Lib3.StatementCommand b) chan
                            respond result
                (Left err, _) -> text $ cs err


-- Response handler
respond :: Either String (Maybe String) -> ActionM ()
respond result = case result of
    Right (Just str) -> text $ cs str
    Left str         -> text $ cs str
    _                -> text "Unknown response"
