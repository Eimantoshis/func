{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Operation(..),
    Entity(..),
    CropNode(..),
    LivestockNode(..),
    Crop(..),
    Livestock(..),
    FarmLocation(..),
    Quantity(..),
    parseWord,
    and2',
    stateTransition,
    parseAlphaNumWhitespaceOrdered,
    State,

    ) where

import qualified Data.Char as C
import qualified Data.List as L



data Query = OperationQuery Operation
    | ShowCropsQuery
    | ShowLivestockQuery
    | ShowFarmQuery
    | ShowPlantedQuery


type Parser a = String -> Either String(a, String)


data Entity
    = Crops CropNode
    | Livestock LivestockNode
    deriving (Eq)

data Operation
    = Add Entity FarmLocation
    | Remove Entity FarmLocation
    | Plant CropNode FarmLocation
    | Harvest CropNode FarmLocation
    deriving (Eq)



data Crop = Wheat | Corn | Soy | Barley
        deriving (Eq, Show)

data FarmLocation = Field String | Barn String
    deriving (Eq, Show)

data Livestock = Cows | Chickens | Sheep | Pigs
        deriving (Eq, Show)

data CropNode = CropLeaf Crop Quantity | CropBranch CropNode CropNode

data LivestockNode = LivestockLeaf Livestock Quantity | LivestockBranch LivestockNode LivestockNode
    deriving (Eq)

data Quantity = Quantity Int
    deriving (Eq)

instance Num Quantity where
    (Quantity x) * (Quantity y) = Quantity (x * y)
    fromInteger x = Quantity (fromInteger x)

instance Show Quantity where
    show (Quantity q) = show q 

instance Show CropNode where
    show (CropLeaf crop (Quantity q)) = show crop ++ " " ++ show q
    show (CropBranch left right) = "(" ++ show left ++ ", " ++ show right ++ ")"

instance Eq CropNode where
    (CropLeaf crop1 qty1) == (CropLeaf crop2 qty2) =
        crop1 == crop2 && qty1 == qty2
    (CropBranch left1 right1) == (CropBranch left2 right2) =
        left1 == left2 && right1 == right2
    _ == _ = False 


instance Show LivestockNode where
    show (LivestockLeaf livestock (Quantity q)) = show livestock ++ " " ++ show q
    show (LivestockBranch left right) = "(" ++ show left ++ ", " ++ show right ++ ")"

instance Show Operation where
    show (Add cropNode field) = "ADD: " ++ show cropNode ++ " TO " ++ show field
    show (Remove cropNode field) = "REMOVE " ++ show cropNode ++ " FROM " ++ show field
    show (Plant cropNode field) = "PLANT " ++ show cropNode ++ " TO " ++ show field
    show (Harvest cropNode field) = "HARVEST " ++ show cropNode ++ " FROM " ++ show field

instance Show Entity where
    show (Crops cropNode) = "Crops: " ++ show cropNode
    show (Livestock livestockNode) = "Livestock: " ++ show livestockNode

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c (h:t) = if c == h then Right (c, t) else Left ("Expected " ++ [c] ++ "But got " ++ [h])

or2 :: Parser a -> Parser a -> Parser a
or2 p1 p2 input =
    case p1 input of
        Left _ -> p2 input  
        success -> success 

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f a b c d = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                                Left _ -> Left "Invalid command"
                        Left _ -> Left "Invalid command"
                Left _ -> Left "Invalid command"
        Left _ -> Left "Invalid command"


and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (f v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1


parseWord :: String -> a -> Parser a
parseWord word value = \input ->
        if take (length word) input == word
        then Right (value, drop (length word) input)
        else Left ("Expected " ++ word ++ ", but found: " ++ input)

-- <Quantity> ::= [0-9]+
parseQuantity :: Parser Int
parseQuantity [] = Left "empty input, cannot parse a number"
parseQuantity str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)


-- <pl> ::= "("
parseOpenParen :: Parser Char
parseOpenParen = parseChar '('


-- <pr> ::= ")"
parseCloseParen :: Parser Char
parseCloseParen = parseChar ')'

parseCommaSpace :: Parser String
parseCommaSpace input =
    if take 2 input == ", "
        then Right (", ", drop 2 input)
        else Left ("Expected ', ', but found: " ++ input)


-- <Entity> ::= <Crops> | <Livestock>
parseEntity :: Parser Entity
parseEntity [] = Left "Cannot parse Entity: input is empty"
parseEntity input =
    case parseCropsEntity input of
        Right result -> Right result
        Left _ ->
            case parseLivestockEntity input of
                Right result -> Right result
                Left err -> Left ("Expected a Crops or Livestock entity, but found: " ++ err)

parseCropsEntity :: Parser Entity
parseCropsEntity input =
    case parseCropNode input of
        Right (cropNode, rest) -> Right (Crops cropNode, rest)
        Left err -> Left err

parseLivestockEntity:: Parser Entity
parseLivestockEntity input =
    case parseLivestockNode input of
        Right (livestockNode, rest) -> Right (Livestock livestockNode, rest)
        Left err -> Left err

-- <Operation> ::= "ADD " <Entity> " TO " <Location> 
--            | "REMOVE " <Entity> " FROM " <Location> 
parseOperation :: Parser Operation
parseOperation [] = Left "Cannot parse Operation: input is empty"
parseOperation input = (parseAdd `or2` parseRemove `or2` parsePlant `or2` parseHarvest) input


parseAdd :: Parser Operation
parseAdd = and4' (\_ entity _ location -> Add entity location)
                 (parseWord "ADD " ())
                 parseEntity
                 (parseWord " TO " ())
                 parseLocation

parseHarvest :: Parser Operation
parseHarvest = and4' (\_ cropNode _ location -> Harvest cropNode location)
                     (parseWord "HARVEST " ())
                     parseCropNode
                     (parseWord " FROM " ())
                     parseLocation

parseRemove :: Parser Operation
parseRemove = and4' (\_ entity _ location -> Remove entity location)
                    (parseWord "REMOVE " ())
                    parseEntity
                    (parseWord " FROM " ())
                    parseLocation

parsePlant :: Parser Operation
parsePlant = and4' (\_ cropNode _ location -> Plant cropNode location)
                   (parseWord "PLANT " ())
                   parseCropNode
                   (parseWord " TO " ())
                   parseLocation





parseLocation :: Parser FarmLocation
parseLocation input =
    case parseField input of
        Right result -> Right result
        Left _ ->
            case parseBarn input of
                Right result -> Right result
                Left err -> Left ("Expected a Field or Barn location, but found: " ++ err)

-- <Location> ::= <Field> | <Barn>
parseField :: Parser FarmLocation
parseField input =
    case parseWord "Field" () input of
        Left err -> Left err
        Right (_, rest) ->
            case parseNumber rest of
                Left err -> Left err
                Right (num, remaining) -> Right (Field ("Field" ++ num), remaining)

parseBarn :: Parser FarmLocation
parseBarn input =
    case parseWord "Barn" () input of
        Left err -> Left err
        Right (_, rest) ->
            case parseNumber rest of
                Left err -> Left err
                Right (num, remaining) -> Right (Barn ("Barn" ++ num), remaining)


-- Helper function to parse numbers (used for Field/Barn names like "Field1", "Barn2")
parseNumber :: Parser String
parseNumber input =
    case parseQuantity input of
        Left err -> Left err
        Right (num, rest) -> Right (show num, rest)

-- <Crop> ::= "Wheat " | "Corn " | "Soy " | "Barley "
parseCrop :: Parser Crop
parseCrop =
    parseWord "Wheat " Wheat `or2`
    parseWord "Corn " Corn `or2`
    parseWord "Soy " Soy `or2`
    parseWord "Barley " Barley

-- <CropLeaf> ::= <Crop> <Quantity>        
parseCropLeaf :: Parser CropNode
parseCropLeaf input =
    case parseCrop input of
        Left err -> Left err
        Right (crop, rest) ->
            let rest' = dropWhile C.isSpace rest
            in case parseQuantity rest' of
                Left err -> Left err
                Right (quantity, remaining) -> Right (CropLeaf crop (Quantity quantity), remaining)


-- <CropBranch> ::= "(" <CropNode> ", " <CropNode> ")"
parseCropBranch :: Parser CropNode
parseCropBranch input =
    case parseOpenParen input of
        Left err -> Left err
        Right (_, rest1) ->
            let rest1' = dropWhile C.isSpace rest1
            in case parseCropNode rest1' of
                Left err -> Left err
                Right (node1, rest2) ->
                    let rest2' = dropWhile C.isSpace rest2
                    in case parseCommaSpace rest2' of
                        Left err -> Left err
                        Right (_, rest3) ->
                            let rest3' = dropWhile C.isSpace rest3
                            in case parseCropNode rest3' of
                                Left err -> Left err
                                Right (node2, rest4) ->
                                    let rest4' = dropWhile C.isSpace rest4
                                    in case parseCloseParen rest4' of
                                        Left err -> Left err
                                        Right (_, remaining) -> Right (CropBranch node1 node2, remaining)

-- <CropNode> ::= <CropBranch> | <CropLeaf>
parseCropNode :: Parser CropNode
parseCropNode = parseCropBranch `or2` parseCropLeaf

-- <LivestockItem> ::= "Cows " | "Chickens " | "Sheep " | "Pigs "
parseLivestock :: Parser Livestock
parseLivestock =
    parseWord "Cows " Cows `or2`
    parseWord "Chickens " Chickens `or2`
    parseWord "Sheep " Sheep `or2`
    parseWord "Pigs " Pigs


-- <LivestockLeaf> ::= <LivestockItem> <Quantity>
parseLivestockLeaf :: Parser LivestockNode
parseLivestockLeaf input =
    case parseLivestock input of
        Left err -> Left err
        Right (livestock, rest) ->
            let rest' = dropWhile C.isSpace rest
            in case parseQuantity rest' of
                Left err -> Left err
                Right (quantity, remaining) -> Right (LivestockLeaf livestock (Quantity quantity), remaining)


-- <LivestockBranch> ::= <pl> <LivestockNode> ", " <LivestockNode> <pr>
parseLivestockBranch :: Parser LivestockNode
parseLivestockBranch input =
    case parseOpenParen input of
        Left err -> Left err
        Right (_, rest1) ->
            let rest1' = dropWhile C.isSpace rest1
            in case parseLivestockNode rest1' of
                Left err -> Left err
                Right (node1, rest2) ->
                    let rest2' = dropWhile C.isSpace rest2
                    in case parseCommaSpace rest2' of
                        Left err -> Left err
                        Right (_, rest3) ->
                            let rest3' = dropWhile C.isSpace rest3
                            in case parseLivestockNode rest3' of
                                Left err -> Left err
                                Right (node2, rest4) ->
                                    let rest4' = dropWhile C.isSpace rest4
                                    in case parseCloseParen rest4' of
                                        Left err -> Left err
                                        Right (_, remaining) -> Right (LivestockBranch node1 node2, remaining)

-- <LivestockNode> ::= <LivestockBranch> | <LivestockLeaf>
parseLivestockNode :: Parser LivestockNode
parseLivestockNode =
    parseLivestockBranch `or2` parseLivestockLeaf



-- | The instances are needed basically for tests
instance Eq Query where
    (==) (OperationQuery op1) (OperationQuery op2) = op1 == op2
    (==) ShowCropsQuery ShowCropsQuery = True
    (==) ShowLivestockQuery ShowLivestockQuery = True
    (==) ShowFarmQuery ShowFarmQuery = True
    (==) _ _ = False

-- Define Show instance for Query for readable output in tests
instance Show Query where
    show (OperationQuery op) = "Operation: " ++ show op
    show ShowCropsQuery = "Show Crops"
    show ShowLivestockQuery = "Show Livestock"
    show ShowFarmQuery = "Show Farm"



---------------------------- STATES -----------------------------------



-- Define the initial State structure
data State = State {
    fieldCrops      :: [(String, [(Crop, Quantity)])],  -- Field name with a list of crops and their quantities
    barnLivestock   :: [(String, [(Livestock, Quantity)])],
    plantedCrops    :: [(String, [(Crop, Quantity)])] 
} deriving (Show)

instance Eq State where
    State fc1 bl1 pc1 == State fc2 bl2 pc2 =
        fc1 == fc2 && bl1 == bl2 && pc1 == pc2


-- Empty state as the initial state
emptyState :: State
emptyState = State [] [] []

-- State transition function
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (OperationQuery (Add entity location)) =
    let newState = addEntityToLocation state entity location
    in Right (Just "Entity added to location!", newState)

stateTransition state (OperationQuery (Remove entity location)) =
    case removeEntityFromLocation state entity location of
        Left err -> Left err 
        Right newState -> Right (Just "Entity removed from location!", newState)


stateTransition state (OperationQuery (Plant cropNode location)) =
    case location of
        Field fieldName -> 
            case removeCropFromField (fieldCrops state) fieldName cropNode of
                Left err -> Left err
                Right updatedFields ->
                    let updatedPlanted = addOrUpdateCropsInField (plantedCrops state) fieldName cropNode
                    in Right (Just "Crops planted!", state { fieldCrops = updatedFields, plantedCrops = updatedPlanted })
        _ -> Left "Invalid location for planting."  -- Handle other location types if necessary


stateTransition state (OperationQuery (Harvest cropNode location)) =
    case location of
        Field fieldName -> 
            case removeCropFromField (plantedCrops state) fieldName cropNode of
                Left err -> Left err
                Right updatedPlanted ->
                    case cropNode of
                        CropLeaf crop quantity -> 
                            -- Calculate the new quantity by multiplying the existing quantity by 4
                            let newQuantity = quantity * 4  -- Ensure 'quantity' is of a compatible type
                                harvestedCrop = CropLeaf crop newQuantity  -- Create a new CropLeaf with the multiplied quantity
                                updatedFields = addOrUpdateCropsInField (fieldCrops state) fieldName harvestedCrop
                            in Right (Just "Crops harvested!", state { fieldCrops = updatedFields, plantedCrops = updatedPlanted })
                        CropBranch left right -> 
                            Left "Harvesting from a CropBranch is not allowed."
        _ -> Left "Invalid location for harvesting."



stateTransition state ShowCropsQuery =
     Right (Just (showCrops state), state)

stateTransition state ShowPlantedQuery =
    Right (Just (showPlanted state), state)

stateTransition state ShowLivestockQuery =
    let summedBarnLivestock = map (\(barn, mlivestock) -> (barn, sumBarnLivestock mlivestock)) (barnLivestock state)
        formattedLivestock = unlines $ concatMap formatBarnLivestock summedBarnLivestock
    in Right (Just formattedLivestock, state)
  where
    formatBarnLivestock :: (String, [(Livestock, Quantity)]) -> [String]
    formatBarnLivestock (barn, mlivestock) =
        [barn ++ ":"] ++ map (\(livestock, Quantity q) -> "  " ++ show livestock ++ " " ++ show q) mlivestock

stateTransition state ShowFarmQuery =
    case (stateTransition state ShowCropsQuery, 
          stateTransition state ShowPlantedQuery, 
          stateTransition state ShowLivestockQuery) of
        (Right (Just crops, _), Right (Just planted, _), Right (Just livestock, _)) ->
            Right (Just ("Crops:\n" ++ crops ++ "\n\nPlanted:\n" ++ planted ++ "\n\nLivestock:\n" ++ livestock), state)
        (Left err, _, _) -> Left err
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
        

showCrops :: State -> String
showCrops state = 
    let summedFieldCrops = map (\(field, crops) -> (field, sumFieldCrops crops)) (fieldCrops state)
        formattedCrops = unlines $ concatMap formatFieldCrops summedFieldCrops
    in formattedCrops
  where
    formatFieldCrops :: (String, [(Crop, Quantity)]) -> [String]
    formatFieldCrops (field, crops) =
        [field ++ ":"] ++ map (\(crop, Quantity q) -> "  " ++ show crop ++ " " ++ show q) crops

showPlanted :: State -> String
showPlanted state =
    let summedPlantedCrops = map (\(field, crops) -> (field, sumFieldCrops crops)) (plantedCrops state)
        formattedPlanted = unlines $ concatMap formatFieldCrops summedPlantedCrops
    in formattedPlanted
  where
    formatFieldCrops :: (String, [(Crop, Quantity)]) -> [String]
    formatFieldCrops (field, crops) =
        [field ++ ":"] ++ map (\(crop, Quantity q) -> "  " ++ show crop ++ " " ++ show q) crops


showLivestock :: State -> String
showLivestock state = "Livestock: " ++ show (barnLivestock state)


getCropQuantity :: CropNode -> Crop -> Int
getCropQuantity (CropLeaf crop (Quantity q)) targetCrop
    | crop == targetCrop = q
    | otherwise          = 0
getCropQuantity (CropBranch left right) targetCrop =
    getCropQuantity left targetCrop + getCropQuantity right targetCrop

getLivestockQuantity :: LivestockNode -> Livestock -> Int
getLivestockQuantity (LivestockLeaf livestock (Quantity q)) targetLivestock
    | livestock == targetLivestock = q
    | otherwise          = 0
getLivestockQuantity (LivestockBranch left right) targetLivestock =
    getLivestockQuantity left targetLivestock + getLivestockQuantity right targetLivestock

addOrUpdateCropsInField :: [(String, [(Crop, Quantity)])] -> String -> CropNode -> [(String, [(Crop, Quantity)])]
addOrUpdateCropsInField [] fieldName cropNode = [(fieldName, flattenAndCombineCrops (flattenCrops cropNode))]
addOrUpdateCropsInField ((fname, crops):rest) fieldName cropNode
    | fname == fieldName =
        let updatedCrops = combineCrops crops (flattenCrops cropNode)
        in (fname, updatedCrops) : rest
    | otherwise = (fname, crops) : addOrUpdateCropsInField rest fieldName cropNode
  where
    -- Helper function to combine crops with the same name by summing quantities
    combineCrops :: [(Crop, Quantity)] -> [(Crop, Quantity)] -> [(Crop, Quantity)]
    combineCrops currentCrops newCrops = flattenAndCombineCrops (currentCrops ++ newCrops)

addOrUpdateLivestockInBarn :: [(String, [(Livestock, Quantity)])] -> String -> LivestockNode -> [(String, [(Livestock, Quantity)])]
addOrUpdateLivestockInBarn [] barnName livestockNode = [(barnName, flattenAndCombineLivestock (flattenLivestock livestockNode))]
addOrUpdateLivestockInBarn ((bname, livestock):rest) barnName livestockNode
    | bname == barnName =
        let updatedLivestock = combineLivestock livestock (flattenLivestock livestockNode)
        in (bname, updatedLivestock) : rest
    | otherwise = (bname, livestock) : addOrUpdateLivestockInBarn rest barnName livestockNode
  where
    -- Helper function to combine livestock with the same name by summing quantities
    combineLivestock :: [(Livestock, Quantity)] -> [(Livestock, Quantity)] -> [(Livestock, Quantity)]
    combineLivestock currentLivestock newLivestock = flattenAndCombineLivestock (currentLivestock ++ newLivestock)

 -- Helper function to flatten and combine crops with the same name in a list
flattenAndCombineCrops :: [(Crop, Quantity)] -> [(Crop, Quantity)]
flattenAndCombineCrops [] = []
flattenAndCombineCrops ((crop, qty):xs) =
    let (sameCrop, differentCrops) = L.partition ((== crop) . fst) xs
        totalQuantity = getQuantity qty + sum (map (getQuantity . snd) sameCrop)
    in (crop, Quantity totalQuantity) : flattenAndCombineCrops differentCrops

flattenAndCombineLivestock :: [(Livestock, Quantity)] -> [(Livestock, Quantity)]
flattenAndCombineLivestock [] = []
flattenAndCombineLivestock ((livestock, qty):xs) =
    let (sameLivestock, differentLivestock) = L.partition ((== livestock) . fst) xs
        totalQuantity = getQuantity qty + sum (map (getQuantity . snd) sameLivestock)
    in (livestock, Quantity totalQuantity) : flattenAndCombineLivestock differentLivestock


getQuantity :: Quantity -> Int
getQuantity (Quantity q) = q


addEntityToLocation :: State -> Entity -> FarmLocation -> State
addEntityToLocation state (Crops cropNode) (Field fieldName) =
    let updatedFields = addOrUpdateCropsInField (fieldCrops state) fieldName cropNode
    in state { fieldCrops = updatedFields }
addEntityToLocation state (Livestock livestockNode) (Barn barnName) =
    let updatedBarns = addOrUpdateLivestockInBarn (barnLivestock state) barnName livestockNode
    in state { barnLivestock = updatedBarns }
addEntityToLocation state _ _ = state  -- For unsupported operations



flattenCrops :: CropNode -> [(Crop, Quantity)]
flattenCrops (CropLeaf crop qty) = [(crop, qty)]
flattenCrops (CropBranch left right) = flattenCrops left ++ flattenCrops right

flattenLivestock :: LivestockNode -> [(Livestock, Quantity)]
flattenLivestock (LivestockLeaf livestock qty) = [(livestock, qty)]
flattenLivestock (LivestockBranch left right) = flattenLivestock left ++ flattenLivestock right

sumFieldCrops :: [(Crop, Quantity)] -> [(Crop, Quantity)]
sumFieldCrops crops =
    let groupedCrops = L.groupBy (\(c1, _) (c2, _) -> c1 == c2) crops
    in map sumGroup groupedCrops
  where
    sumGroup :: [(Crop, Quantity)] -> (Crop, Quantity)
    sumGroup group =
        let (crop, quantities) = (fst (head group), map (\(_, Quantity q) -> q) group)
        in (crop, Quantity (sum quantities))

sumBarnLivestock :: [(Livestock, Quantity)] -> [(Livestock, Quantity)]
sumBarnLivestock livestock =
    let groupedlivestock = L.groupBy (\(l1, _) (l2, _) -> l1 == l2) livestock
    in map sumGroup groupedlivestock
  where
    sumGroup :: [(Livestock, Quantity)] -> (Livestock, Quantity)
    sumGroup group =
        let (livestock, quantities) = (fst (head group), map (\(_, Quantity q) -> q) group)
        in (livestock, Quantity (sum quantities))



-- Helper function to remove an entity from a location
removeEntityFromLocation :: State -> Entity -> FarmLocation -> Either String State
removeEntityFromLocation state (Crops cropNode) (Field fieldName) =
    case removeCropFromField (fieldCrops state) fieldName cropNode of
        Left err -> Left err
        Right updatedFields -> Right $ state { fieldCrops = updatedFields }

removeEntityFromLocation state (Livestock livestockNode) (Barn barnName) =
    case removeLivestockFromBarn (barnLivestock state) barnName livestockNode of
        Left err -> Left err
        Right updatedBarns -> Right $ state { barnLivestock = updatedBarns }

-- function to remove a crop from a field
removeCropFromField :: [(String, [(Crop, Quantity)])] -> String -> CropNode -> Either String [(String, [(Crop, Quantity)])]
removeCropFromField [] _ _ = Left "Field not found"
removeCropFromField ((fname, crops) : rest) fieldName cropNode
    | fname == fieldName = 
        case removeCrops crops (flattenCrops cropNode) of
            Left err -> Left err
            Right updatedCrops -> 
                let filteredCrops = filter (\(_, Quantity q) -> q > 0) updatedCrops
                in if null filteredCrops 
                   then Right rest  -- If no crops remain, remove the field entirely
                   else Right ((fname, filteredCrops) : rest)
    | otherwise = fmap ((fname, crops) :) (removeCropFromField rest fieldName cropNode)


removeLivestockFromBarn :: [(String, [(Livestock, Quantity)])] -> String -> LivestockNode -> Either String [(String, [(Livestock, Quantity)])]
removeLivestockFromBarn [] _ _ = Left "Barn not found"
removeLivestockFromBarn ((bname, livestock) : rest) barnName livestockNode
    | bname == barnName =
        case removeLivestock livestock (flattenLivestock livestockNode) of
            Left err -> Left err
            Right updatedLivestock ->
                let filteredLivestock = filter (\(_, Quantity q) -> q > 0) updatedLivestock
                in if null filteredLivestock
                   then Right rest  -- If no livestock remain, remove the barn entirely
                   else Right ((bname, filteredLivestock) : rest)
    | otherwise = fmap ((bname, livestock) :) (removeLivestockFromBarn rest barnName livestockNode)


-- function to remove a specific crop quantity
removeCrops :: [(Crop, Quantity)] -> [(Crop, Quantity)] -> Either String [(Crop, Quantity)]
removeCrops crops [] = Right crops
removeCrops crops ((crop, qty) : toRemove) =
    case adjustQuantityCrop crops crop qty of
        Left err -> Left err
        Right updatedCrops -> removeCrops updatedCrops toRemove


removeLivestock :: [(Livestock, Quantity)] -> [(Livestock, Quantity)] -> Either String [(Livestock, Quantity)]
removeLivestock mlivestock [] = Right mlivestock
removeLivestock mlivestock ((livestock, qty) : toRemove) =
    case adjustQuantityLivestock mlivestock livestock qty of
        Left err -> Left err
        Right updatedLivestock -> removeLivestock updatedLivestock toRemove

-- Helper function to adjust quantities of specific crops
adjustQuantityCrop :: [(Crop, Quantity)] -> Crop -> Quantity -> Either String [(Crop, Quantity)]
adjustQuantityCrop [] _ _ = Left "Crop not found or insufficient quantity"
adjustQuantityCrop ((c, Quantity q) : rest) crop (Quantity qty)
    | c == crop && q >= qty = Right $ (c, Quantity (q - qty)) : rest
    | c == crop && q < qty = Left "Insufficient quantity of crop"
    | otherwise = fmap ((c, Quantity q) :) (adjustQuantityCrop rest crop (Quantity qty))

adjustQuantityLivestock :: [(Livestock, Quantity)] -> Livestock -> Quantity -> Either String [(Livestock, Quantity)]
adjustQuantityLivestock [] _ _ = Left "Livestock not found or insufficient quantity"
adjustQuantityLivestock ((l, Quantity q) : rest) livestock (Quantity qty)
    | l == livestock && q >= qty = Right $ (l, Quantity (q - qty)) : rest
    | l == livestock && q < qty = Left "Insufficient quantity of livestock"
    | otherwise = fmap ((l, Quantity q) :) (adjustQuantityLivestock rest livestock (Quantity qty))


parseQuery :: String -> Either String Query
parseQuery input
  | input == "" = Left "Cannot parse empty input"
  | input == "SHOW PLANTED" = Right ShowPlantedQuery
  | input == "SHOW CROPS" = Right ShowCropsQuery
  | input == "SHOW LIVESTOCK" = Right ShowLivestockQuery
  | input == "SHOW FARM" = Right ShowFarmQuery
  | otherwise = case parseOperation input of
      Right (operation, rest) -> Right (OperationQuery operation)
      Left errorMsg           -> Left errorMsg


parseWhitespace :: Parser Char
parseWhitespace [] = Left "Cannot find any whitespace in an empty input"
parseWhitespace s@(h:t) = if ' ' == h then Right (' ', t) else Left (s ++ " does not start with a whitespace")

parseAlphaNum :: Parser String
parseAlphaNum [] = Left "Cannot find any text made out of letters or digits in an empty input"
parseAlphaNum str =
    let
        lettersDigits = L.takeWhile (\c -> C.isLetter c || C.isDigit c) str
        rest = drop (length lettersDigits) str
    in
        case lettersDigits of
            [] -> Left "Not a letter or a digit"
            _ -> Right (lettersDigits, rest)

parseAlphaNumWhitespaceOrdered :: Parser String
parseAlphaNumWhitespaceOrdered = and2' (\a _ -> a) parseAlphaNum parseWhitespace

