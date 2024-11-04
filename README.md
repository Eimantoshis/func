# fp-2024 


**Open the Project in a Dev Container:**
> Command + Shift + P (MacOS) \
> Dev Containers: Open Folder in Container


**Build and Test**
> stack build \
> stack test


**Try Lecture Code**
> ghci src/Lessons/Lesson01.hs

**Run Lab2**
> stack run fp2024-two


## DOMAIN - FARM 

**Main Entities:**

* Crops: Various types of plants managed on the farm.
* Livestock: Animals raised on the farm.

**Main Operations:**

* Add/Remove Crops or Livestock: Add new crops or livestock to the system or remove them.

* Plant or Harvest Crops, where harvesting gives more back than planting.

**Code example**
* REMOVE LIVESTOCK: (Cows 4, (Chickens 98, Chickens 0)) FROM BARN: Barn1 
* ADD LIVESTOCK: ((Pigs 79, (Cows 5, Pigs 22)), Pigs 8) to BARN: Barn2 
* PLANT Wheat 831 TO Barn1 
* HARVEST ((Corn 1, Wheat 1), Barley 3) FROM Field2 





# CHANGES IN LAB2

* Removed Inventory and its command Restock as it is worse version of add/remove.

* Removed "CROP: ", "LIVESTOCK: ", "FIELD: ", "BARN: " for better visability since it is obvious and looks redundant

* Added "PLANT" and "HARVEST" Operations 