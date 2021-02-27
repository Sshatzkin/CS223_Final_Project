module Msg exposing (..)

import Plants exposing (Plant)

type Msg
    = NoOp
    | Reset
    | Increment
    | Frame Float
    | BuyPlant Plant Int -- Sends a plant and the cost of the plant
    | SellPlant Int Plant