{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements(..),
    applyStatements,
    Command(..),
    ) where

import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import Control.Concurrent.STM(STM, TVar, writeTVar, atomically)
import qualified Lib2
import Control.Monad (void)

import Control.Exception (try, IOException)
import Data.List (intercalate)
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Debug.Trace (trace)

import Data.Either (isLeft, isRight, rights, partitionEithers)



data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan Lib3.StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of 
    Save content askChan -> do
      result <- try (writeFile "SaveState.txt" content) :: IO (Either IOException ())
      case result of
        Left ex -> do
          putStrLn $ "Error saving to file: " ++ show ex
        Right _ -> do
          putStrLn "Successfully saved content"
      writeChan askChan ()
      storageOpLoop chan
    
    Load replyChan -> do
      result <- try (readFile "SaveState.txt") :: IO (Either IOException String)
      case result of
        Left ex -> do
          putStrLn $ "Error loading from file: " ++ show ex
          writeChan replyChan ""
        Right content -> do
          putStrLn "Successfully loaded content from file"
          writeChan replyChan content
      storageOpLoop chan




data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = 
      case parseKeyword input of
        Left err -> Left err 
        Right ("Save", rest) -> 
            Right (SaveCommand, rest)  
        Right ("Load", rest) -> 
            Right (LoadCommand, rest) 
        Right (_, _) ->
          case parseStatements input of
            Right(stmt, r) -> Right (StatementCommand stmt, r)
            Left e2 -> Left e2

parseKeyword :: String -> Either String (String, String)
parseKeyword input =
    let (keyword, rest) = span isAlphaNum input
    in if null keyword
        then Left "Expected a command keyword"
        else Right (keyword, dropWhile (== ' ') rest)

isAlphaNum :: Char -> Bool
isAlphaNum c = c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.


parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
    let linesOfInput = lines input
    in case linesOfInput of
        ("BEGIN" : rest) ->
            case break (== "END") rest of
                (queries, "END" : remaining) -> 
                    case parseListOfStatements (unlines queries) of
                        Right (parsedQueries, _) -> Right (Batch parsedQueries, unlines remaining)
                        Left err                 -> Left $ "Error parsing queries:\n" ++ err
                _ -> Left "Missing END statement in input."
        (singleLine : rest) ->
            case Lib2.parseQuery singleLine of
                Right query -> Right (Single query, unlines rest)
                Left err    -> Left $ "Error parsing single query: " ++ err
        [] -> Left "Empty input."


-- This code was good for debugging 
--      |
--      |
--      |
--      V

-- parseStatements :: String -> Either String (Statements, String)
-- parseStatements input = 
--     let linesOfInput = splitLines input -- Split input into lines manually
--     in trace ("Input lines: " ++ show linesOfInput) $
--         case linesOfInput of
--             ("BEGIN" : rest) -> 
--                 --trace "Processing BEGIN block" $
--                 case break (== "END") rest of
--                     (queries, "END" : remaining) -> 
--                         --trace ("Parsing queries: " ++ show queries) $
--                         case parseListOfStatements (unlines queries) of
--                             Right (parsedQueries, _) -> 
--                                 --trace "Successfully parsed queries" $
--                                 Right (Batch parsedQueries, unlines remaining)
--                             Left err -> 
--                                 --trace ("Error parsing queries: " ++ err) $
--                                 Left $ "Error parsing queries:\n" ++ err
--                     _ -> 
--                         --trace "Missing END statement" $
--                         Left "Missing END statement in input."
--             (singleLine : rest) -> 
--                 --trace ("Processing single query: " ++ singleLine) $
--                 case Lib2.parseQuery singleLine of
--                     Right query -> 
--                         --trace "Successfully parsed single query" $
--                         Right (Single query, unlines rest)
--                     Left err -> 
--                         --trace ("Error parsing single query: " ++ err) $
--                         Left $ "Error parsing single query: " ++ err
--             [] -> 
--                 --trace "Input is empty" $
--                 Left "Empty input."

-- splitLines :: String -> [String]
-- splitLines input = lines (filter (`notElem` "\r") input) 




parseListOfStatements :: String -> Either String ([Lib2.Query], String)
parseListOfStatements input =
    let parsedLines = map Lib2.parseQuery (lines input)
    in case partitionEithers parsedLines of
        ([], queries) -> Right (queries, "") 
        (errors, _)   -> Left (unlines errors) 






-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
-- | Converts program's state into Statements (likely a batch).
-- | Converts program's state into Statements (likely a batch).
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State fieldCrops barnLivestock plantedCrops) =
    Batch (fieldStatements ++ barnStatements ++ plantedStatements)
  where
    marshalField :: (String, [(Lib2.Crop, Lib2.Quantity)]) -> [Lib2.Query]
    marshalField (fieldName, crops) =
        [Lib2.OperationQuery (Lib2.Add (Lib2.Crops (Lib2.CropLeaf crop qty)) (Lib2.Field fieldName))
        | (crop, qty) <- crops]

    marshalBarn :: (String, [(Lib2.Livestock, Lib2.Quantity)]) -> [Lib2.Query]
    marshalBarn (barnName, livestock) =
        [Lib2.OperationQuery (Lib2.Add (Lib2.Livestock (Lib2.LivestockLeaf animal qty)) (Lib2.Barn barnName))
        | (animal, qty) <- livestock]


    marshalPlanted :: (String, [(Lib2.Crop, Lib2.Quantity)]) -> [Lib2.Query]
    marshalPlanted (fieldName, crops) =
        [Lib2.OperationQuery (Lib2.Plant (Lib2.CropLeaf crop qty) (Lib2.Field fieldName))
        | (crop, qty) <- crops]



    fieldStatements = concatMap marshalField fieldCrops
    barnStatements = concatMap marshalBarn barnLivestock
    plantedStatements = concatMap marshalPlanted plantedCrops



    





-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements statements =
  case statements of
    Single query -> renderQuery query
    Batch queries ->
      "BEGIN\n" ++ unlines (map renderQuery queries) ++ "END\n"


renderQuery :: Lib2.Query -> String
renderQuery query =
  case query of
    Lib2.OperationQuery operation ->  renderOperation operation
    Lib2.ShowCropsQuery           -> "SHOW CROPS"
    Lib2.ShowLivestockQuery       -> "SHOW LIVESTOCK"
    Lib2.ShowFarmQuery            -> "SHOW FARM"
    Lib2.ShowPlantedQuery         -> "SHOW PLANTED"

renderOperation :: Lib2.Operation -> String
renderOperation op =
  case op of
    Lib2.Add (Lib2.Crops (Lib2.CropLeaf crop qty)) (Lib2.Field fieldName) ->
      "ADD " ++ show crop ++ " " ++ show qty ++ " TO " ++ fieldName
    Lib2.Add (Lib2.Livestock (Lib2.LivestockLeaf livestock qty)) (Lib2.Barn barnName) ->
      "ADD " ++ show livestock ++ " " ++ show qty ++ " TO " ++ barnName
    Lib2.Plant (Lib2.CropLeaf crop qty) (Lib2.Field fieldName) ->
      "ADD " ++ show crop ++ " " ++ show qty ++ " TO " ++ fieldName ++ "\n" ++
      "PLANT " ++ show crop ++ " " ++ show qty ++ " TO " ++ fieldName
    _ -> "UNKNOWN OPERATION"





    

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> 
                   IO (Either String (Maybe String))
stateTransition stateVar command ioChan = do
  case command of
    StatementCommand (Batch queries) -> do
      results <- atomically $ mapM (processSingleQuery stateVar) queries
      let resultMessages = map (either id (maybe "" id)) results
      let combinedMessage = unlines resultMessages
      if any isLeft results
        then return $ Left "One or more queries in the batch failed."
        else return $ Right $ Just $ "Batch of queries processed successfully.\n" ++ combinedMessage

    StatementCommand (Single query) -> do
      result <- atomically $ processSingleQuery stateVar query
      case result of
        Left err -> return $ Left err
        Right Nothing -> return $ Right Nothing
        Right (Just msg) -> return $ Right $ Just ("Success: \n" ++ msg)

    LoadCommand -> do
      responseChan <- newChan
      writeChan ioChan (Load responseChan)
      loadedStateAsString <- readChan responseChan
      case parseStatements loadedStateAsString of
        Right (parsedStatements, _) -> case parsedStatements of
          Batch queries -> atomically $ do
            -- Reset the state to an empty state before applying queries
            writeTVar stateVar Lib2.emptyState
            results <- mapM (processSingleQuery stateVar) queries
            let resultMessages = map (either id (maybe "" id)) results
            let combinedMessage = unlines resultMessages
            if any isLeft results
              then return $ Left "One or more queries in the loaded batch failed."
              else return $ Right $ Just ("State loaded and batch applied successfully.\n" ++ combinedMessage)
          Single query -> atomically $ do
            -- Reset the state to an empty state before applying the query
            writeTVar stateVar Lib2.emptyState
            result <- processSingleQuery stateVar query
            case result of
              Left err -> return $ Left err
              Right Nothing -> return $ Right Nothing
              Right (Just msg) -> return $ Right $ Just ("State loaded with a single query.\n" ++ msg)
        Left err -> return $ Left $ "Failed to parse loaded state: " ++ err

    SaveCommand -> do
      currentState <- readTVarIO stateVar
      let statementsAsString = renderStatements $ marshallState currentState
      responseChan <- newChan
      writeChan ioChan (Save statementsAsString responseChan)
      _ <- readChan responseChan
      return $ Right $ Just "State saved"



processSingleQuery :: TVar Lib2.State -> Lib2.Query -> STM (Either String(Maybe String))
processSingleQuery givenState query = do
  currentState <- readTVar givenState
  case Lib2.stateTransition currentState query of 
    Right (msg, updatedState) -> do
      writeTVar givenState updatedState
      case msg of
        Just str -> return $ Right $ Just str
        Nothing -> return $ Right Nothing
    Left err -> return $ Left err

printQueryResponse :: Either String (Maybe String) -> IO ()
printQueryResponse (Left err) = putStrLn $ "Failed: \n" ++ err
printQueryResponse (Right Nothing) = putStrLn "Success: No message returned"
printQueryResponse (Right (Just msg)) = putStrLn $ "Success: \n" ++ msg


applyStatements :: [Lib2.Query] -> Lib2.State -> Lib2.State
applyStatements queries initialState =
    foldl applyQuery initialState queries
  where
    applyQuery :: Lib2.State -> Lib2.Query -> Lib2.State
    applyQuery state query =
        case Lib2.stateTransition state query of
            Right (_, newState) -> newState
            Left _ -> state  

