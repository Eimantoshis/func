<Farm> ::= <Fields> <Barn> <Inventory>

<Operation> ::= "ADD " <Entity> " to " <Location> | "REMOVE " <Entity> " from " <Location> | "RESTOCK " <Inventory> <Quantity>

<Entity> ::= <Crops> | <Livestock> 

<Crops> ::= "CROP: " <Crop> | "CROPS: " <Crop> ", " <CropList>

<CropList> ::= <Crop> | <Crop> ", " <CropList>
<Crop> ::= "Wheat" | "Corn" | "Soy" | "Barley"

<Livestock> ::= "LIVESTOCK: " <LivestockItem> | "LIVESTOCK: " <LivestockItem> ", " <LivestockList>

<LivestockList> ::= <LivestockItem> | <LivestockItem> ", " <LivestockList>
<LivestockItem> ::= "Cows" | "Chickens" | "Sheep" | "Pigs"

<Inventory> ::= "INVENTORY: " <ItemList>

<ItemList> ::= <Item> | <Item> ", " <ItemList>
<Item> ::= <Seeds> | <Feed>

<Location> ::= <Field> | <Barn>

<Field> ::= "FIELD: " <FieldName> | <Field> " FIELD SECTION " <Field> | <Field> <Crops>

<Barn> ::= "BARN: " <BarnName> | <Barn> " BARN SECTION " <Barn> | <Barn> <Livestock>

<Seeds> ::= "Corn" | "Wheat"
<Feed> ::= "Hay" | "Grains"

<Quantity> ::= "QUANTITY: " <Number>
<Number> ::= [0-9]+

<FieldName> ::= "Field1" | "Field2" | "Field3"
<BarnName> ::= "Barn1" | "Barn2" | "Barn3"
