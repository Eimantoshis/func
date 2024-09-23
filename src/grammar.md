<Farm> ::= <Field> <Barn> <Inventory>

<Operation> ::= "ADD " <Entity> " to " <Location> | "REMOVE " <Entity> " FROM " <Location> | "RESTOCK " <Inventory> 

<Entity> ::= <Crops> | <Livestock> 

<Crops> ::= "CROP: " <Crop> <Quantity> | "CROPS: "  <CropNode>

<CropNode> ::= <CropBranch> | <CropLeaf>
<CropBranch> ::= <CropNode> ", " <CropNode>
<CropLeaf> ::= <Crop> <Quantity>

<Crop> ::= "Wheat " | "Corn " | "Soy " | "Barley "

<Livestock> ::= "LIVESTOCK: " <LivestockNode>

<LivestockNode> ::= <LivestockBranch> | <LivestockLeaf>
<LivestockBranch> ::= <LivestockNode> ", " <LivestockNode>
<LivestockLeaf> ::= <LivestockItem> <Quantity>

<LivestockItem> ::= "Cows " | "Chickens " | "Sheep " | "Pigs "

<Inventory> ::= "INVENTORY: " <ItemNode>

<ItemNode> ::= <ItemBranch> | <ItemLeaf>
<ItemBranch> ::= <ItemNode> ", " <ItemNode>
<ItemLeaf> ::= <Item> <Quantity>

<Item> ::= <Seeds> | <Feed>

<Location> ::= <Field> | <Barn>

<Field> ::= "FIELD: " <FieldName> 

<Barn> ::= "BARN: " <BarnName>

<Seeds> ::= "Corn " | "Wheat "
<Feed> ::= "Hay " | "Grains "

<Quantity> ::= [0-9]+

<FieldName> ::= "Field1 " | "Field2 " | "Field3 "
<BarnName> ::= "Barn1 " | "Barn2 " | "Barn3 "
