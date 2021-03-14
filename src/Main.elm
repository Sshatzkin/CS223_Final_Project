module Main exposing (..)

import Browser
import Browser.Events as BEvents
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html exposing (Html, text)
import Html.Attributes exposing (id)
import Html.Events as Events

-- Our Libraries
import Button exposing (clickedAnyButton, BType(..))
import Model exposing (..)
import Msg exposing (Msg(..))
import Page exposing (Page(..))
import Plants as P
import Plants exposing (Plant)
import ViewHelpers as VH
import Html

-- TODO FIGURE OUT HOW TO DO LOCAL STORAGE THANKS

{-
  The main of the entire application
-}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-
  This function is called by the main on start
-}
init : Flags -> ( Model, Cmd Msg )
init flag =
    ( initModel flag, Cmd.none )

---- UPDATE ----

{-
  This is used to detect key strokes
-}
keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

{-
  This is called by main to establish subscriptions.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BEvents.onAnimationFrameDelta Frame   -- This sub updates on every frame (this is our frame cycle)
        , BEvents.onClick (Decode.succeed NoOp) -- This sub updates on click
        , BEvents.onKeyDown   -- This sub updates on key down - case for key
            (Decode.map
                (\key ->
                    if key == "Escape" then
                        Reset

                    else
                        NoOp
                )
                (Decode.field "key" Decode.string)
            )
        ]

{-
  This function is called by main every time a new Msg is sent. Case for Msg.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      Frame _ ->  
      -- Called on every animation frame
        ({model | frame = model.frame + 1, plants = P.agePlants model.plants }, Cmd.none)

      Click event ->
        let
          _ = Debug.log "Event" event.offsetPos
          (x, y) = event.offsetPos
        in   
      -- Called when we need a "NoOp"
        ( clickHandler x y model, Cmd.none)

      Reset -> 
      -- Called to reset player coins
        ({ model | coins = 0}, Cmd.none )

      BuyPlant p ->
      -- Called by plant purchase buttons to initiate plant purchase
        if (p.price > model.coins)
        then
          (model, Cmd.none)
        else
          ({ model | plants = (P.addPlant p model.plants), coins = model.coins - p.price}, Cmd.none)
      
      SellPlant index plant ->
      -- Called when a plant is harvested
        ({model | coins = model.coins + (P.value plant), plants = P.removePlant index model.plants}, Cmd.none) 

      AddCoins val ->
      -- Called to add or subtract a given value from the player's total money
        ( { model | coins = model.coins + val }, Cmd.none)
      
      ChangePage pg ->
        ( { model | page = pg }, Cmd.none)

      NoOp ->   
      -- Called when we need a "NoOp"
        ( model, Cmd.none )

clickHandler : Float -> Float -> Model -> Model 
clickHandler x y model =
  case (clickedAnyButton x y model.page model.buttons) of 
    Nothing -> model
    Just btype -> 
      let
        _ = Debug.log "Clicked Button" btype
      in
        case btype of 
          Plot ptype -> 
            case P.plotClicked model.plants ptype of
              Nothing -> model
              Just (plants, profit) -> {model | plants = plants, coins = model.coins + profit}
          _ -> model


-- VIEW
{-
  Called by the main to create the Html view.
-}

view : Model -> Html Msg
view model =
  Html.div
    [ id "game"]
    [ gameView model
    , Html.span 
          [ id "frameRate"] 
          [ text ("Frame: " ++ String.fromFloat model.frame)]
    ]

{-
  Determines which game page should be displayed
-}
gameView : Model -> Html Msg
gameView model = 
  case model.page of
    Farm -> VH.displayFarm model.frame model.window model.coins model.plants
    Store -> VH.displayStore model.coins