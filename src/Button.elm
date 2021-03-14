module Button exposing (
    Button, Buttons, BType (..)
    , initialButtons, clickedAnyButton, getButtonPage)

import Page exposing (Page(..))
import Plants exposing (Plant, PType, getPTypes)
import List

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

-- Makes a new button
newButton : Float -> Float -> Float -> Float -> BType -> Button
newButton x y width height btype =
  { x = x
  , y = y
  , width = width 
  , height = height
  , btype = btype
  }

-- Generates the plant plot buttons
plotButtons : Float -> Float -> List PType -> List Button
plotButtons width height ptypes =
  let
    numPlotsPerRow = 3
    numRows = 2
    plotwidth = 100
    plotheight = 100
    upgradeHeight = 20
    widthMult = (width / numPlotsPerRow)
    xShift = (widthMult - plotwidth) / 2 
    yShift = height / 4

    heightMult = (height - yShift) / (toFloat numRows)
    _ = Debug.log "Height" heightMult

    makePlot : Int -> PType -> Button
    makePlot i p =
      newButton 
        (((toFloat (modBy numPlotsPerRow i)) * widthMult) + xShift) 
        ((heightMult) * (toFloat (i // numPlotsPerRow)) + yShift)
        (toFloat (plotwidth)) 
        (toFloat (plotheight)) 
        (Plot p)
    
    makeUpgrade : Int -> PType -> Button
    makeUpgrade i p =
      newButton
        (((toFloat (modBy numPlotsPerRow i)) * widthMult) + xShift) 
        ((heightMult) * (toFloat (i // numPlotsPerRow)) + yShift + plotheight + 5)
        (toFloat (plotwidth)) 
        (toFloat (upgradeHeight)) 
        (Upgrade p)

    makeButtons i p =
      [makePlot i p, makeUpgrade i p]
  in
    List.concat (List.indexedMap makeButtons ptypes)

-- Generates all buttons on the farm page (plot buttons and menu buttons)
farmButtons : Float -> Float -> List PType -> ButtonPage
farmButtons width height plants = 
  { page = Farm , buttons = (plotButtons width height plants)}

initialButtons : Float -> Float -> List Plant -> Buttons
initialButtons width height plants =
  let
    _ = Debug.log "Farm Button Page " (farmButtons width height (getPTypes plants))
  in
    [farmButtons width height (getPTypes plants)]


---- Button Handling Functions
getButtonPage : Page -> Buttons -> List Button
getButtonPage page bpages =
  List.concat 
    <| List.filterMap (\ bpage -> if (bpage.page == page) then (Just bpage.buttons) else Nothing) bpages
 

clickedButton : Float -> Float -> Button -> Maybe BType
clickedButton x y button =
  if (x > button.x) && (x < (button.x + button.width)) && (y > button.y) && (y < (button.y + button.height))
  then (Just button.btype)
  else Nothing

clickedAnyButton : Float -> Float -> Page -> Buttons -> Maybe BType
clickedAnyButton x y page bpages =
  let
    buttons = getButtonPage page bpages
    btypes = List.filterMap (clickedButton x y) buttons
  in
    case btypes of
      [] -> Nothing
      hd::[] -> (Just hd)
      _ -> Debug.todo "TWO OR MORE BUTTONS WERE CLICKED THIS SHOULD NOT HAPPEN"
