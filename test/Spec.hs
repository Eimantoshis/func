{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified  -- Assuming Lib1 is relevant to your project
import Lib2 qualified




main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
    [
    testCase "Parsing case 1 - Parse empty Query" $
      Lib2.parseQuery "" @?= Left "Cannot parse empty input",

    testCase "Parsing case 2 - Parsing show crops operation" $
      Lib2.parseQuery "SHOW CROPS" @?= Right Lib2.ShowCropsQuery,

    testCase "Parsing case 3 - Parsing incorrect operation" $
      Lib2.parseQuery "ADD (Wheat 10) TO Field1" @?= Left "Invalid command",


    testCase "Parsing case 4 - Parsing single add Operation" $
      Lib2.parseQuery "ADD Wheat 10 TO Field1" @?= Right (Lib2.OperationQuery (Lib2.Add (Lib2.Crops (Lib2.CropLeaf Lib2.Wheat (Lib2.Quantity 10))) (Lib2.Field "Field1"))),

    testCase "Parsing case 5 - Parsing branch add Operation " $
      Lib2.parseQuery "ADD (Wheat 10, Corn 3) TO Field2" @?= Right(Lib2.OperationQuery(Lib2.Add(Lib2.Crops(Lib2.CropBranch(Lib2.CropLeaf Lib2.Wheat (Lib2.Quantity 10))(Lib2.CropLeaf Lib2.Corn (Lib2.Quantity 3))))(Lib2.Field "Field2"))),

    testCase "Parsing case 6 - Parsing single remove Operation" $
      Lib2.parseQuery "REMOVE Chickens 10 FROM Barn1" @?= Right (Lib2.OperationQuery (Lib2.Remove (Lib2.Livestock (Lib2.LivestockLeaf Lib2.Chickens (Lib2.Quantity 10))) (Lib2.Barn "Barn1"))),

    testCase "Parsing case 5 - Parsing add Operation branch" $
      Lib2.parseQuery "ADD (Chickens 10, Sheep 5) TO Barn2" @?= Right(Lib2.OperationQuery(Lib2.Add(Lib2.Livestock(Lib2.LivestockBranch(Lib2.LivestockLeaf Lib2.Chickens (Lib2.Quantity 10))(Lib2.LivestockLeaf Lib2.Sheep (Lib2.Quantity 5))))(Lib2.Barn "Barn2")))


    ]
