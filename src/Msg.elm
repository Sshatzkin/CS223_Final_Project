module Msg exposing (..)
import Html.Events.Extra.Mouse as Mouse


import Plants exposing (Plant)
import Page exposing (Page)
type Msg
    = NoOp
    | Reset
    | Frame Float
    | Click Mouse.Event
    | BuyPlant Plant -- Sends a plant
    | SellPlant Int Plant
    | AddCoins Int
    | ChangePage Page