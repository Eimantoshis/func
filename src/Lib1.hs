module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- Entities
    "FARM",
    "CROPS",
    "Field",
    "Barn",
    "LIVESTOCK",

    -- Crops
    "Wheat",
    "Corn",
    "Soy",
    "Barley",

    -- Livestock
    "Cows",
    "Sheep",
    "Chickens",
    "Pigs",

    -- Commands
    "ADD",
    "REMOVE"
    ]
