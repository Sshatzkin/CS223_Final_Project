module Window exposing (..)

type alias Window = 
  { width : Float 
  , height : Float
  }

type alias PlotSize = 
  { width : Float
  , height : Float}
newWindow : Float -> Float -> Window
newWindow w h = {width = w, height = h}

newPlotSize : Float -> Float -> PlotSize
newPlotSize w h = {width = w, height = h}