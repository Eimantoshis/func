<Farm> ::= <Field> <Barn>

<Operation> ::= "ADD " <Entity> " TO " <Location> 
              | "REMOVE " <Entity> " FROM " <Location> 
              | "PLANT " <CropNode> " TO " <Location>
              | "HARVEST " <CropNode> " FROM " <Location>

<Entity> ::= <Crops> | <Livestock>

<Crops> ::= <Crop> <Quantity> 
          |  <CropNode> 


<CropNode> ::= <CropBranch> | <CropLeaf>
<CropBranch> ::= <pl> <CropNode> ", " <CropNode> <pr>    
<CropLeaf> ::= <Crop> <Quantity>                        

<Crop> ::= "Wheat " | "Corn " | "Soy " | "Barley "

<Livestock> ::= <LivestockNode>

<LivestockNode> ::= <LivestockBranch> | <LivestockLeaf>
<LivestockBranch> ::= <pl> <LivestockNode> ", " <LivestockNode> <pr>
<LivestockLeaf> ::= <LivestockItem> <Quantity>

<LivestockItem> ::= "Cows " | "Chickens " | "Sheep " | "Pigs "

<Location> ::= <Field> | <Barn>

<Field> ::= <FieldName> 
<Barn> ::=  <BarnName>

<Quantity> ::= [0-9]+

<FieldName> ::= "Field1 " | "Field2 " | "Field3 "
<BarnName> ::= "Barn1 " | "Barn2 " | "Barn3 "

<pl> ::= "("
<pr> ::= ")"



