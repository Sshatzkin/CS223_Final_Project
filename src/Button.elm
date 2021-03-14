module Button exposing (
    Button, Buttons, BType (..)
    , initialButtons, clickedAnyButton, getButtonPage)

import Page exposing (Page(..))
import Plants exposing (Plant, PType, getPTypes)
import List

-- BType == Button type, determines what will happen when button clicked
type BType 
  = Plot PType
  | Upgrade PType

-- Button - stores all button info
type alias Button = 
  { x : Float 
  , y : Float
  , width : Float
  , height : Float
  , btype : BType
  }

-- ButtonPage - stores all buttons on a Page.
type alias ButtonPage = 
  { page : Page
  , buttons : List Button
  }

{- Buttons - stores All button pages in the application
  There is a Buttons in the model
-}
type alias Buttons =
  List ButtonPage

---- BUTTON GENERATING FUNCTIONS ----

-- Makes a new button
newButton : Float -> Float -> Float -> Float -> BType -> Button
newButton x y width height btype =
  { x = x
  , y = y
  , width = width 
  , height = height
  , btype = btype
  }

{-
  Generates all buttons on the farm page (called on game initialization)

  Args:
    width, height -> window width and height 
    ptypes -> A list of ptypes (plant identifiers) that will be assigned to 
      each button

  Returns ->
    a Farm button page
-}
farmButtons : Float -> Float -> List PType -> ButtonPage
farmButtons width height ptypes =
  let
    -- Define a few constants for setting button locations
    numPlotsPerRow = 3
    numRows = 2
    plotwidth = 100
    plotheight = 100
    upgradeHeight = 20
    widthMult = (width / numPlotsPerRow)  -- The "spacing" between each col's x-values
    xShift = (widthMult - plotwidth) / 2 -- The buffer before the first col
    yShift = height / 4 -- The buffer between the top and the first row

    heightMult = (height - yShift) / (toFloat numRows) -- The "spacing" between each row's y-values
    _ = Debug.log "Height" heightMult

    -- This function generates a plot button
    makePlot : Int -> PType -> Button
    makePlot i p =
      newButton 
        (((toFloat (modBy numPlotsPerRow i)) * widthMult) + xShift) 
        ((heightMult) * (toFloat (i // numPlotsPerRow)) + yShift)
        (toFloat (plotwidth)) 
        (toFloat (plotheight)) 
        (Plot p)

    -- This function generates an upgrade button
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
    { page = Farm , buttons = List.concat (List.indexedMap makeButtons ptypes)}

  
-- Initializes the model's buttons field
initialButtons : Float -> Float -> List Plant -> Buttons
initialButtons width height plants =
  -- Right now, we only have one page of buttons
  [farmButtons width height (getPTypes plants)]


---- BUTTON HANDLING FUNCTIONS ----

{-
  Searches a Buttons for a specific Page and returns the list of buttons from that page
-}
getButtonPage : Page -> Buttons -> List Button
getButtonPage page bpages =
  List.concat 
    <| List.filterMap (\ bpage -> if (bpage.page == page) then (Just bpage.buttons) else Nothing) bpages
 

{-
  Checks if a button was clicked

  Args:
    x, y -> Click coordinates
    button -> the button

  Returns:
    Nothing if not clicked | the button's BType if clicked
-}
clickedButton : Float -> Float -> Button -> Maybe BType
clickedButton x y button =
  if (x > button.x) && (x < (button.x + button.width)) && (y > button.y) && (y < (button.y + button.height))
  then (Just button.btype)
  else Nothing

{-
  Checks if any button on a page was clicked

  Args:
    x, y -> Click coordinates
    page -> current page
    bpages -> all buttons in the model

  Returns:
    Nothing if no button clicked | the button's BType if a button is clicked
-}
clickedAnyButton : Float -> Float -> Page -> Buttons -> Maybe BType
clickedAnyButton x y page bpages =
  let
    -- Get buttons just from the current page 
    buttons = getButtonPage page bpages

    -- Get the btypes of clicked buttons by checking if each was clicked
    btypes = List.filterMap (clickedButton x y) buttons
  in
    case btypes of
      [] -> Nothing -- None clicked
      hd::[] -> (Just hd) -- Just one button clicked
      _ -> Debug.todo "TWO OR MORE BUTTONS WERE CLICKED THIS SHOULD NOT HAPPEN" -- More than one button clicked
