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
  , price : Int --The initial price of the plant (if not yet purchased)
  , value : Int --The current value of the plant (for selling)
  , countdown : Int --The countdown until the plant is mature
  , matAge : Int --The age at which the plant is ready to harvest
  , ptype : PType -- The PlantType!
  , purchased : Bool --Determines whether or not the player has made an initial purchase
  , quantity : Int --The number of the plant the player has purchased
  , upgradePrice : Float --The price of each subsequent plant purchase
  }

type PType 
  = Corn
  | Tomato
  | Pumpkin
  | Carrot
  | Radish
  | Pepper

{-
  Initial state of plants in model

  All plants are added at beginning of the game

  The order of the plants here is the order in which they will appear in the game
-}
initPlants : List Plant
initPlants = [corn, tomato, pumpkin, carrot, radish, pepper]

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
  , purchased = False -- Initialized to False
  , quantity = 0 -- Initialized to 0
  , upgradePrice = upgradeScale 0 (toFloat p)
  }

-- PLANTS LIST --
{-
  We can define a list of plants here for quick use elsewhere
-}
corn : Plant
corn = newPlant Corn "Corn" 1 3 300 

tomato : Plant
tomato = newPlant Tomato "Tomato" 3 5 600

pumpkin : Plant
pumpkin = newPlant Pumpkin "Pumpkin" 5 10 1500

carrot : Plant
carrot = newPlant Carrot "Carrot" 10 20 1700

radish : Plant
radish = newPlant Radish "Radish" 100 300 2200

pepper : Plant
pepper = newPlant Pepper "Pepper" 3000 5000 4000


---- GET / SET FUNCTIONS ----

{-
  Takes a plant identifier and a plant and returns True if this plant is of same type
-}
isType :  PType -> Plant -> Bool
isType ptype plant =
  plant.ptype == ptype

{-
  Takes plant identifier and list of plants, returns correct plant

  Args:
    ptype -> Plant identifier
    plants -> List of plants

  Returns:
    Plant with correct name
-}
getPlant : PType -> List Plant -> Plant
getPlant ptype plants = 
  case List.filter (isType ptype) plants of
    [] -> Debug.todo "Why can't this find the plant?"
    hd::[] -> hd
    _::_ -> Debug.todo "THIS SHOULD NOT BE POSSIBLE"

{-
  Takes plant identifier and list of plants and a plant function, 
    returns result of function applied to correct plant

  Args:
    func -> A function of type (Plant -> a), i.e. 'value' or 'isGrown'
    ptype -> Plant identifier
    plants -> List of plants

  Returns:
    Value of function applied to plant
-}
plantGet : (Plant -> a) -> PType -> List Plant -> a
plantGet func ptype plants  =
  func (getPlant ptype plants)
 

{-
  Takes plant identifier and list of plants and a plant-altering function, 
    returns list of plants with the correct modified plant

  Args:
    func -> A function of type (Plant -> Plant), i.e. 'resetAge' or 'upgradePlant'
    ptype -> Plant identifier
    plants -> List of plants

  Returns:
    List of plants with correct modification to specific plant
-}
plantSet : (Plant -> Plant) -> PType -> List Plant -> List Plant
plantSet func ptype plants =
  List.map
  (\ p -> 
    if (p.ptype == ptype)
    then func p
    else p)
  plants

{-
  Gets all the ptypes from a list of plants (used to initialize the buttons)
-}
getPTypes : List Plant -> List PType
getPTypes plants =
  List.map (\ p -> p.ptype ) plants

{-
  Gets the value (without upgrades) from a Plant object. 
-}
value : Plant -> Int
value p =
  p.value

{-
  Gets the value (with upgrades) from a Plant object
-}
harvestValue : Plant -> Int
harvestValue p =
  p.value * p.quantity

{-
  Gets price of a plant
-}
price : Plant -> Int
price p =
  p.price

{-
  Checks if a given plant is fully grown
-}
isGrown : Plant -> Bool
isGrown plant =
  plant.countdown == 0


---- UPDATE FUNCTIONS ----

{-
  Updates the age of a plant by one frame
  
  Args: 
    p - plant
  
  Returns:
    Plant with updated age field
-}
agePlant : Plant -> Plant
agePlant p =
  if p.purchased
  then {p | countdown = max (p.countdown - 1) 0}
  else p

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

{-
  Resets the age of a plant (called when plant is harvested)
-}
resetAge : Plant -> Plant
resetAge p =
  {p | countdown = p.matAge}

{-
  Switches plant between purchased and not (called when plant purchased)
-}
togglePurchase : Plant -> Plant
togglePurchase p =
  {p | purchased = not p.purchased, quantity = 1}

{-
  Defines the rate at which the price of an upgrade increases
  
  Args: 
    lvl -> Current quantity of plant owned
    oldPrice -> The price of the upgrade at the previous level
  
  Returns:
    Price of the upgrade at the next level
-}
upgradeScale : Int -> Float -> Float
upgradeScale lvl oldPrice =
  oldPrice * 1.10

{-
  Upgrades a specific plant
  
  Args: 
    p - plant
  
  Returns:
    Plants with upgrade executed 
      (quantity increased, upgradePrice increased, other features executed)
-}
upgradePlant : Plant -> Plant
upgradePlant p =
  let
    level = p.quantity
  in
    case (modBy 10 level) of
      -- Every 10 levels of upgrade, decrease grow time by 0.75 (you can change these numbers)
       0 -> {p | quantity = level + 1, upgradePrice = upgradeScale level p.upgradePrice, matAge = round ((toFloat p.matAge) * 0.75)}
       _ -> {p | quantity = level + 1, upgradePrice = upgradeScale level p.upgradePrice}

{-
  Responds to clicking on a plot -> 
    Either attempts to purchase or attempts to harvest
  
  Args: 
    plants - List of plants in model
    ptype - Name of plant in the plot that was clicked
    coins - Current coins that the player has 
  
  Returns:
    If plot can be purchased -> Just (updated plants, - purchase price)
    If plot can be harvested -> Just (updated plants, harvest value)
    Else -> Nothing
-}
plotClicked : List Plant -> PType -> Int -> Maybe (List Plant, Int)
plotClicked plants ptype coins =
  let
    plant = getPlant ptype plants
  in
    if (plant.purchased)
    then
      if (isGrown plant)
      then Just (plantSet resetAge ptype plants, harvestValue plant)
      else Nothing
    else
      if (coins > plant.price)
      then Just (plantSet togglePurchase ptype plants, -plant.price)
      else Nothing


{-
  Responds to clicking on an upgrade button -> 
    Attempts to purchase upgrade
  
  Args: 
    plants - List of plants in model
    ptype - Name of plant in the plot that was clicked
    coins - Current coins that the player has 
  
  Returns:
    If upgrade can be purchased -> Just (updated plants, - purchase price)
    Else -> Nothing
-}
upgradeClicked : List Plant -> PType -> Int -> Maybe ((List Plant), Int)
upgradeClicked plants ptype coins =
  let
    plant = getPlant ptype plants
  in
    if (plant.purchased)
    then
      if (coins >= (round plant.upgradePrice))
      then Just (plantSet upgradePlant ptype plants, -(round plant.upgradePrice))
      else Nothing
    else Nothing
      

