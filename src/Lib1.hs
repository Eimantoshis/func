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
    "animal",
    "tractor",
    "equipment",
    "planting-schedule",
    "harvest-schedule",
    "inventory",
    "seed",
    "fertilizer",
    "feed",
    "weather",
    "market-price",

    -- Crops
    "Wheat",
    "Corn",
    "Soybean",
    "Rice",
    "Potatoes",
    "Vegetables",
    "Fruits",

    -- Livestock
    "Cows",
    "Sheep",
    "Chickens",
    "Pigs",
    "Goats",

    -- Commands
    "addCrop",
    "removeCrop",
    "plantCrop",
    "harvestCrop",
    "addLivestock",
    "feedLivestock",
    "checkWeather",
    "trackMarketPrices",
    "updateInventory",
    "schedulePlanting",
    "scheduleHarvest"
    ]

