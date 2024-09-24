# fp-2024 


**Open the Project in a Dev Container:**
> Command + Shift + P (MacOS) \
> Dev Containers: Open Folder in Container


**Build and Test**
> stack build \
> stack test


**Try Lecture Code**
> ghci src/Lessons/Lesson01.hs


## DOMAIN - FARM 

**Main Entities:**

* Crops: Various types of plants managed on the farm.
* Livestock: Animals raised on the farm.
* Inventory: Seeds, feed, etc.

**Main Operations:**

* Add/Remove Crops or Livestock: Add new crops or livestock to the system or remove them.

**Code example**
* FIELD: Field1 BARN: Barn1 INVENTORY: (Corn 0), ((Corn 92), (Hay 23))
* RESTOCK INVENTORY: ((Wheat 4), ((((Hay 66), (Wheat 1)), (Corn 9)), ((Corn 04), (Corn 5)))), (Grains 3)
* ADD CROPS: (Corn 2), ((Wheat 7), (Soy 580)) to BARN: Barn1 
* REMOVE LIVESTOCK: ((Sheep 9), (((Cows 947), (Cows 7)), ((Pigs 6), (Pigs 2)))), (Sheep 257) FROM FIELD: Field3 