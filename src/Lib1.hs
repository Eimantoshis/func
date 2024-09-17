module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- Entities
    "farm",
    "crop",
    "field",
    "barn",
    "livestock",
    "inventory",
    "seed",
    "fertilizer",
    "feed",

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
    "Add",
    "Remove",
    "Restock",
    "Plant",
    "Harvest",
    "Feed"
    ]

