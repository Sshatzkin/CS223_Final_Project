module Plants exposing (..)
{-
  The plants module contains all plant-related functions. Importantly, we define
  the Plant type alias, and all functions that manipulate and update plants
-}

import Html
import Html exposing (Html, text, div, br)
import Html.Attributes exposing (id, class)

type alias Plant = 
  { name : String
  , price : Int --The initial price of the plant (for buying)
  , value : Int --The current value of the plant (for selling)
  , countdown : Int --The countdown until the plant is mature
  , matAge : Int --The age at which the plant is ready to harvest
  , ptype : PType -- The PlantType!
  }

type PType 
  = Corn
  | Pumpkin

{-
  Initial state of plants field in model
-}
initPlants : List Plant
initPlants = [corn, pumpkin]

{-
  Creates a new plant 
  
  Args: 
    name - plant name
    val - plant value
    matAge - age of maturity of the plant
  
  Returns:
    Plant with initialized with given values
-}
newPlant : PType -> String -> Int -> Int -> Int -> Plant
newPlant ptype name p val matAge =
  { name = name
  , price = p
  , value = val
  , countdown = matAge
  , matAge = matAge
  , ptype = ptype
  }

-- PLANTS LIST --
{-
  We can define a list of plants here for quick use elsewhere
-}

corn : Plant
corn = newPlant Corn "Corn" 1 3 400 

pumpkin : Plant
pumpkin = newPlant Pumpkin "Pumpkin" 5 10 2000


-- GET / SET FUNCTIONS --
isType :  PType -> Plant -> Bool
isType ptype plant =
  plant.ptype == ptype

getPlant : PType -> List Plant -> Maybe Plant
getPlant ptype plants = 
  case List.filter (isType ptype) plants of
    [] -> Nothing
    hd::[] -> Just hd
    _::_ -> Debug.todo "THIS SHOULD NOT BE POSSIBLE"

{-
  Takes a plant name and a function that takes a plant and returns an a
-}
plantGet : (Plant -> a) -> PType -> List Plant -> a
plantGet func ptype plants  =
  case getPlant ptype plants of
    Nothing -> Debug.todo "Cannot apply function - plant not in list"
    Just plant -> func plant

{-
  Takes a plant name and a function that alters that plant
-}
plantSet : (Plant -> Plant) -> PType -> List Plant -> List Plant
plantSet func ptype plants =
  List.map
  (\ p -> 
    if (p.ptype == ptype)
    then func p
    else p)
  plants

getPTypes : List Plant -> List PType
getPTypes plants =
  List.map (\ p -> p.ptype ) plants
{-
  Gets the value from a Plant object. 
-}
value : Plant -> Int
value p =
  p.value

valueFromList : List Plant -> PType -> Int
valueFromList plants ptype =
  plantGet value ptype plants

price : Plant -> Int
price p =
  p.price

priceFromList : List Plant  -> PType -> Int
priceFromList plants ptype =
  plantGet price ptype plants

-- UPDATE FUNCTIONS -- 
{-
  Updates the age of a plant by one frame
  
  Args: 
    p - plant
  
  Returns:
    Plant with updated age field
-}
agePlant : Plant -> Plant
agePlant p =
  {p | countdown = max (p.countdown - 1) 0}

{-
  Updates the age of a list of plants
  
  Args: 
    ps - plant list
  
  Returns:
    Plants with updated age field
-}
agePlants : List Plant -> List Plant
agePlants ps =
  List.map (\n -> agePlant n) ps

resetAge : Plant -> Plant
resetAge p =
  {p | countdown = p.matAge}

{-
  Adds a plant to a list of plants
  
  Args: 
    p - plant
    ps - plant list
  
  Returns:
    Plants with the new plant appended
-}
addPlant : Plant -> List Plant -> List Plant
addPlant p ps =
  p::ps

removePlant : Int -> List Plant -> List Plant
removePlant i ps =
  List.append (List.take i ps) (List.drop (i+1) ps)

isGrown : Plant -> Bool
isGrown plant =
  plant.countdown == 0

plotClicked : List Plant -> PType -> Maybe ((List Plant), Int)
plotClicked plants ptype =
  let
    pvalue = valueFromList plants ptype
  in
    if (plantGet isGrown ptype plants)
    then Just (plantSet resetAge ptype plants, pvalue)
    else Nothing
