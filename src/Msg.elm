module Msg exposing (..)
import Html.Events.Extra.Mouse as Mouse


import Plants exposing (Plant)
import Page exposing (Page)
type Msg
    = NoOp
    | Reset -- Reset coins to 0
    | Frame Float -- Called on every frame - used to age plants and animate
    | Click Mouse.Event -- Called on every mouse click, stores click info
    | AddCoins Int
    | ChangePage Page