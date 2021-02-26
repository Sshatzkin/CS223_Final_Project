module Plants exposing (..)

import Html
import Html exposing (Html, text, div, br)
import Html.Attributes exposing (id)

type alias Plant = 
  { name : String
  , value : Int
  , age : Int 
  , matAge : Int
  }

initPlants : List Plant
initPlants = []

newPlant : String -> Int -> Int -> Plant
newPlant name val matAge =
   { name = name
  , value = val
  , age = matAge
  , matAge = matAge
  }

-- PLANTS LIST --

corn : Plant
corn = newPlant "Corn" 3 500

pumpkin : Plant
pumpkin = newPlant "Pumpkin" 10 2000


-- GET / SET FUNCTIONS --

value : Plant -> Int
value p =
  p.value


-- UPDATE FUNCTIONS -- 

agePlant : Plant -> Plant
agePlant p =
  {p | age = max (p.age - 1) 0}

agePlants : List Plant -> List Plant
agePlants ps =
  List.map (\n -> agePlant n) ps

addPlant : Plant -> List Plant -> List Plant
addPlant p ps =
  p::ps

-- DISPLAY FUNCTIONS -- 

displayPlant : Plant -> Html msg
displayPlant p = 
  div 
    []
    [ text ("Name: " ++ p.name ++ " ")
    , text ("Value: " ++ String.fromInt p.value ++ " ")
    , text ("Age: " ++ String.fromInt (p.matAge - p.age) ++ ".")]


plantsView : List Plant -> Html msg
plantsView ps =
  div 
  []
  (List.foldl (\n acc -> (displayPlant n)::acc) [] ps)

