{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad.Free (Free (..), liftF)
import Data.ByteString (ByteString)
import Network.Wreq
import Data.String.Conversions
import Control.Lens

data FarmAlgebra next =
      Load (() -> next)
    | Save (() -> next)
    | Batch [String] (() -> next)
    deriving Functor

type Farmtype = Free FarmAlgebra

load :: Farmtype ()
load = liftF $ Load id

save :: Farmtype ()
save = liftF $ Save id

batch :: [String] -> Farmtype ()
batch queries = liftF $ Batch queries id

interpretSingle :: Farmtype a -> IO a
interpretSingle (Pure a) = return a
interpretSingle (Free cmd) = do
    case cmd of
        Load next -> do
            putStrLn "Sending LOAD command..."
            resp <- post "http://localhost:3000/upload" (cs "Load" :: ByteString)
            putStrLn $ "Response: " ++ cs (resp ^. responseBody)
            interpretSingle (next ())
        Save next -> do
            putStrLn "Sending SAVE command..."
            resp <- post "http://localhost:3000/upload" (cs "Save" :: ByteString)
            putStrLn $ "Response: " ++ cs (resp ^. responseBody)
            interpretSingle (next ())
        Batch cmds next -> do
            putStrLn "Sending Command..."
            let body = cs $ concatMap (\cmd -> cmd ++ "\n") cmds :: ByteString
            resp <- post "http://localhost:3000/upload" body
            putStrLn $ "Response: " ++ cs (resp ^. responseBody)
            interpretSingle (next ())

-- Interpret batch commands
interpretBatch :: Farmtype a -> IO a
interpretBatch = go []
  where
    go :: [String] -> Farmtype a -> IO a
    go acc (Pure a) = do
        flushBatch acc 
        return a
    go acc (Free cmd) =
        case cmd of
            Load next -> do
                flushBatch acc
                putStrLn "Sending LOAD command..."
                resp <- post "http://localhost:3000/upload" (cs "Load" :: ByteString)
                putStrLn $ "Response: " ++ cs (resp ^. responseBody)
                go [] (next ())
            Save next -> do
                flushBatch acc
                putStrLn "Sending SAVE command..."
                resp <- post "http://localhost:3000/upload" (cs "Save" :: ByteString)
                putStrLn $ "Response: " ++ cs (resp ^. responseBody)
                go [] (next ())
            Batch cmds next -> do
                go (acc ++ cmds) (next ())

    flushBatch :: [String] -> IO ()
    flushBatch [] = return ()
    flushBatch cmds = do
        putStrLn "Sending accumulated BATCH commands..."
        let body = cs $ ("BEGIN\n" ++ unlines cmds ++ "END") :: ByteString
        resp <- post "http://localhost:3000/upload" (cs body :: ByteString)
        putStrLn $ "Response: " ++ cs (resp ^. responseBody)

interpretInMemory :: Farmtype a -> IO a
interpretInMemory (Pure a) = return a
interpretInMemory (Free cmd) = do
   case cmd of
       Load next -> do
        putStrLn "Simulating LOAD in memory..."
        interpretInMemory (next ())
       Save next -> do
           putStrLn "Simulating SAVE in memory..."
           interpretInMemory (next ())
       Batch cmds next -> do
           putStrLn "Simulating BATCH in memory..."
           interpretInMemory (next ())

-- Example program to interpret
exampleProgram :: Farmtype ()
exampleProgram = do
    ----------SINGLE----------------
    load
    batch ["ADD Wheat 10 TO Field1"]
    batch ["PLANT Wheat 6 TO Field1"]
    batch ["ADD Sheep 10 TO Barn2"]
    batch ["SHOW FARM"]

    -----------BATCHES---------------
    -- batch ["ADD (Corn 6, (Wheat 9, (Soy 2, Barley 1123))) TO Field1\nADD (Wheat 1, Corn 4) TO Field1\nPLANT Corn 5 TO Field1"]
    -- batch ["ADD (Sheep 10, (Pigs 15, Cows 26)) TO Barn1\nADD (Chickens 2, Sheep 1) TO Barn1\nSHOW FARM"]
    -- save

main :: IO ()
main = do
    putStrLn "Running with Single Command Interpreter:"
    interpretSingle exampleProgram

    -- putStrLn "Running with Batching Interpreter:"
    -- interpretBatch exampleProgram

    -- interpretInMemory exampleProgram
    -- putStrLn "In-Memory Interpretation Successful"
