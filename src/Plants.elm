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
  }

{-
  Initial state of plants field in model
-}
initPlants : List Plant
initPlants = []

{-
  Creates a new plant 
  
  Args: 
    name - plant name
    val - plant value
    matAge - age of maturity of the plant
  
  Returns:
    Plant with initialized with given values
-}
newPlant : String -> Int -> Int -> Int -> Plant
newPlant name p val matAge =
  { name = name
  , price = p
  , value = val
  , countdown = matAge
  , matAge = matAge
  }

-- PLANTS LIST --
{-
  We can define a list of plants here for quick use elsewhere
-}

corn : Plant
corn = newPlant "Corn" 1 3 400

pumpkin : Plant
pumpkin = newPlant "Pumpkin" 5 10 2000


-- GET / SET FUNCTIONS --
{-
  We use these functions to abstract away the content of a Plant type.
-}

{-
  Gets the value from a Plant object. 
-}
value : Plant -> Int
value p =
  p.value

price : Plant -> Int
price p =
  p.price

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

