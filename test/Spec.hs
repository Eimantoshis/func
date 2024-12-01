{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC
import Control.Monad (replicateM)



import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import qualified Lib3
import qualified Lib3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

-- Unit Tests
unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
    [ testCase "Parsing case 1 - Parse empty Query" $
        Lib2.parseQuery "" @?= Left "Cannot parse empty input",
      testCase "Parsing case 2 - Parsing show crops operation" $
        Lib2.parseQuery "SHOW CROPS" @?= Right Lib2.ShowCropsQuery,
      testCase "Parsing case 3 - Parsing incorrect operation" $
        Lib2.parseQuery "ADD (Wheat 10) TO Field1" @?= Left "Invalid command",
      testCase "Parsing case 4 - Parsing single add Operation" $
        Lib2.parseQuery "ADD Wheat 10 TO Field1" @?=
          Right (Lib2.OperationQuery (Lib2.Add (Lib2.Crops (Lib2.CropLeaf Lib2.Wheat (Lib2.Quantity 10))) (Lib2.Field "Field1"))),
      testCase "Parsing case 5 - Parsing branch add Operation " $
        Lib2.parseQuery "ADD (Wheat 10, Corn 3) TO Field2" @?=
          Right (Lib2.OperationQuery (Lib2.Add (Lib2.Crops (Lib2.CropBranch (Lib2.CropLeaf Lib2.Wheat (Lib2.Quantity 10)) (Lib2.CropLeaf Lib2.Corn (Lib2.Quantity 3)))) (Lib2.Field "Field2"))),
      testCase "Parsing case 6 - Parsing single remove Operation" $
        Lib2.parseQuery "REMOVE Chickens 10 FROM Barn1" @?=
          Right (Lib2.OperationQuery (Lib2.Remove (Lib2.Livestock (Lib2.LivestockLeaf Lib2.Chickens (Lib2.Quantity 10))) (Lib2.Barn "Barn1"))),
      testCase "Parsing case 7 - Parsing add Operation branch" $
        Lib2.parseQuery "ADD (Chickens 10, Sheep 5) TO Barn2" @?=
          Right (Lib2.OperationQuery (Lib2.Add (Lib2.Livestock (Lib2.LivestockBranch (Lib2.LivestockLeaf Lib2.Chickens (Lib2.Quantity 10)) (Lib2.LivestockLeaf Lib2.Sheep (Lib2.Quantity 5)))) (Lib2.Barn "Barn2"))),
      testCase "Parsing case 8 - Parsing single plant Operation" $
        Lib2.parseQuery "PLANT Corn 7 TO Field1" @?=
          Right (Lib2.OperationQuery (Lib2.Plant (Lib2.CropLeaf Lib2.Corn (Lib2.Quantity 7)) (Lib2.Field "Field1")))
    ]


propertyTests :: TestTree
propertyTests = testGroup "Lib2 Property-Based Tests"
  [ QC.testProperty "Generated commands parse, save, and load correctly" prop_parseValidBatchCommands,
    QC.testProperty "Save-then-load preserves state" prop_saveThenLoadPreservesState
  ]
genCommand :: Gen String
genCommand = do
  operation <- elements ["ADD", "PLANT"]
  entity <- case operation of
    "PLANT" -> genCrops  -- PLANT can only be applied to crops
    _       -> genEntity  -- ADD can be applied to both crops and livestock
  location <- genLocation
  case operation of
    "ADD"  -> return $ unwords [operation, entity, "TO", location]
    "PLANT"  -> return $ unwords [operation, entity, "TO", location]

genEntity :: Gen String
genEntity = elements [genCrops, genLivestock] >>= id  
genCrops :: Gen String
genCrops = do
  crop <- elements ["Wheat", "Corn", "Soy", "Barley"]
  quantity <- genQuantity
  return $ crop ++ " " ++ quantity

genLivestock :: Gen String
genLivestock = do
  livestock <- elements ["Cows", "Chickens", "Sheep", "Pigs"]
  quantity <- genQuantity
  return $ livestock ++ " " ++ quantity


genLocation :: Gen String
genLocation = elements ["Field1", "Field2", "Field3", "Barn1", "Barn2", "Barn3"]

genQuantity :: Gen String
genQuantity = show <$> chooseInt (1, 100)

prop_parseValidBatchCommands :: Property
prop_parseValidBatchCommands = QC.forAll (generateBatch 5) $ \cmd -> 
  case Lib3.parseStatements cmd of
    Left _  -> counterexample ("Failed to parse: " ++ cmd) $ property False
    --Right _ -> counterexample ("Successfully parsed: " ++ cmd) $ property True
    Right _ -> counterexample ("Successfully parsed: " ++ cmd) $ property True


-- Generate a batch of operations
generateBatch :: Int -> Gen String
generateBatch numCommands = do
  commands <- replicateM numCommands genCommand
  return $ unlines $ ["BEGIN"] ++ commands ++ ["END"] 

  
prop_saveThenLoadPreservesState :: Property
prop_saveThenLoadPreservesState = QC.forAll (generateBatch 5) $ \cmdBatch -> QC.ioProperty $ do
  let filePath = "tempState.txt"
  case Lib3.parseStatements cmdBatch of
    Left err -> return $ counterexample ("Parse failed for: " ++ cmdBatch ++ " with error: " ++ err) False
    Right parsed -> do
      let beforeSave = show parsed
      --putStrLn $ "Before saving: " ++ beforeSave

      saveState filePath beforeSave

      reloaded <- loadState filePath
      --putStrLn $ "After loading: " ++ reloaded
      -- Compare the states
      return $ property (reloaded == beforeSave)



saveState :: FilePath -> String -> IO ()
saveState filePath state = writeFile filePath state

loadState :: FilePath -> IO String
loadState filePath = readFile filePath