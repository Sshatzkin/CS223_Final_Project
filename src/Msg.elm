module Msg exposing (..)

import Plants exposing (Plant)

type Msg
    = NoOp
    | Reset
    | Increment
    | Frame Float
    | BuyPlant Plant Int
    | SellPlant Int Plant