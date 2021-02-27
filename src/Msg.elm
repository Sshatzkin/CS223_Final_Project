module Msg exposing (..)

import Plants exposing (Plant)

type Msg
    = NoOp
    | Reset
    | Increment
    | Frame Float
    | BuyPlant Plant -- Sends a plant
    | SellPlant Int Plant
    | AddCoins Int 