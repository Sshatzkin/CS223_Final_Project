module Button exposing (
    Button, Buttons, BType (..)
    , initialButtons, clickedAnyButton)

import Page exposing (Page(..))
import Plants exposing (Plant, PType, getPTypes)

type BType 
  = Plot PType
  | Unlock PType
  | Upgrade PType

type alias Button = 
  { x : Float 
  , y : Float
  , width : Float
  , height : Float
  , btype : BType
  }

type alias ButtonPage = 
  { page : Page
  , buttons : List Button
  }

type alias Buttons =
  List ButtonPage

--
newButton : Float -> Float -> Float -> Float -> BType -> Button
newButton x y width height btype =
  { x = x
  , y = y
  , width = width 
  , height = height
  , btype = btype
  }

plotButtons : Float -> Float -> List PType -> List Button
plotButtons width height plants =
  let
    makePlot : Int -> PType -> Button
    makePlot i p =
      newButton 
        (toFloat ((i * 20) + 5)) 
        (toFloat (i * 0))
        (toFloat (i * 10)) 
        (toFloat (i * 10)) 
        (Plot p)
  in
    List.indexedMap makePlot plants

farmButtons : Float -> Float -> List PType -> ButtonPage
farmButtons width height plants = 
  { page = Farm , buttons = (plotButtons width height plants)}

initialButtons : Float -> Float -> List Plant -> Buttons
initialButtons width height plants =
  [farmButtons width height (getPTypes plants)]


---- Button Handling Functions
clickedButton : Float -> Float -> Button -> Maybe BType
clickedButton x y button =
  if (x > button.x) && (x < (button.x + button.width)) && (y > button.y) && (y < (button.y + button.height))
  then (Just button.btype)
  else Nothing

clickedAnyButton : Float -> Float -> Page -> Buttons -> Maybe BType
clickedAnyButton x y page bpages =
  let
    buttons = 
      List.concat 
        <| List.filterMap (\ bpage -> if (bpage.page == page) then (Just bpage.buttons) else Nothing) bpages
    btypes = List.filterMap (clickedButton x y) buttons
  in
    case btypes of
      [] -> Nothing
      hd::[] -> (Just hd)
      _ -> Debug.todo "TWO OR MORE BUTTONS WERE CLICKED THIS SHOULD NOT HAPPEN"
