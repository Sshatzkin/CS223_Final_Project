module Msg exposing (..)

import Plants exposing (Plant)
import Page exposing (Page)
type Msg
    = NoOp
    | Reset
    | Frame Float
    | BuyPlant Plant -- Sends a plant
    | SellPlant Int Plant
    | AddCoins Int
    | ChangePage Page