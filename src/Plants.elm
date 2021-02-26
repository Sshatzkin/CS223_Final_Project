module Plants exposing (..)
{-
  The plants module contains all plant-related functions. Importantly, we define
  the Plant type alias, and all functions that manipulate and update plants
-}

import Html
import Html exposing (Html, text, div, br)
import Html.Attributes exposing (id)

type alias Plant = 
  { name : String
  , value : Int
  , age : Int 
  , matAge : Int
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
newPlant : String -> Int -> Int -> Plant
newPlant name val matAge =
   { name = name
  , value = val
  , age = matAge
  , matAge = matAge
  }

-- PLANTS LIST --
{-
  We can define a list of plants here for quick use elsewhere
-}

corn : Plant
corn = newPlant "Corn" 3 500

pumpkin : Plant
pumpkin = newPlant "Pumpkin" 10 2000


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
  {p | age = max (p.age - 1) 0}

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

-- DISPLAY FUNCTIONS -- 

{-
  Converts a Plant to an Html msg that can be displayed
  
  Args: 
    p - plant
  
  Returns:
    The visual representation of the plant.
-}
displayPlant : Plant -> Html msg
displayPlant p = 
  div 
    []
    [ text ("Name: " ++ p.name ++ " ")
    , text ("Value: " ++ String.fromInt p.value ++ " ")
    , text ("Age: " ++ String.fromInt (p.matAge - p.age) ++ ".")]

{-
  Converts a list of plants to a viewable layout
  
  Args: 
    ps - list of plants
  
  Returns:
    The visual representation of all of a player's plants.
-}
plantsView : List Plant -> Html msg
plantsView ps =
  div 
  []
  (List.foldl (\n acc -> (displayPlant n)::acc) [] ps)

