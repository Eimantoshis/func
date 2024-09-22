<Farm> ::= <Field> <Barn> <Inventory>

<Operation> ::= "ADD " <Entity> " to " <Location> | "REMOVE " <Entity> " FROM " <Location> | "RESTOCK " <Inventory> 

<Entity> ::= <Crops> | <Livestock> 

<Crops> ::= "CROP: " <Crop> <Quantity> | "CROPS: " <Crop> <Quantity> ", " <CropList>

<CropList> ::= <Crop> <Quantity> | <Crop> <Quantity> ", " <CropList>
<Crop> ::= "Wheat " | "Corn " | "Soy " | "Barley "

<Livestock> ::= "LIVESTOCK: " <LivestockItem> <Quantity> | "LIVESTOCK: " <LivestockItem> <Quantity> ", " <LivestockList>

<LivestockList> ::= <LivestockItem>  <Quantity> | <LivestockItem> <Quantity> ", " <LivestockList>
<LivestockItem> ::= "Cows " | "Chickens " | "Sheep " | "Pigs "

<Inventory> ::= "INVENTORY: " <ItemList> <Quantity>

<ItemList> ::= <Item> <Quantity> | <Item> <Quantity> ", " <ItemList>
<Item> ::= <Seeds> | <Feed>

<Location> ::= <Field> | <Barn>

<Field> ::= "FIELD: " <FieldName> 

<Barn> ::= "BARN: " <BarnName>

<Seeds> ::= "Corn " | "Wheat "
<Feed> ::= "Hay " | "Grains "

<Quantity> ::= [0-9]+

<FieldName> ::= "Field1 " | "Field2 " | "Field3 "
<BarnName> ::= "Barn1 " | "Barn2 " | "Barn3 "
