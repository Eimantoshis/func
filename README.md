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
* Inventory: Seeds, fertilizers, feed, etc.
* Schedules: For planting, harvesting, and maintenance.

**Main Operations:**

* Add/Remove Crops or Livestock: Add new crops or livestock to the system or remove them.
* Manage Inventory: Track inventory of seeds, fertilizers, and feed.
* Schedule Operations: Set and modify schedules for planting, harvesting, feeding, etc.
* Track Weather: Incorporate weather data to adjust schedules.
* Market Monitoring: Track market prices for better decision-making.


**Recursive elements**

* A Field can contain other Fields (subfields).
* A Farm can have multiple Barns, and a Barn can have multiple Livestock.


**Code example**

* AddCrop "Corn" FieldA
* ScheduleHarvest "Corn" "01-11-2024"
* AddLivestock "Cow" Barn1
* RestockInventory "Fertilizer" 100
