module Main exposing (..)

import Browser
import Browser.Events as BEvents
import Json.Decode as Decode
import Html
import Html exposing (Html, text)
import Html.Attributes exposing (id)

-- Our Libraries
import Button exposing (clickedAnyButton, BType(..))
import Model exposing (..)
import Msg exposing (Msg(..))
import Page exposing (Page(..))
import Plants as P
import ViewHelpers as VH
import Html

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
      -- Called on every click
        let
          _ = Debug.log "Event" event.offsetPos
          (x, y) = event.offsetPos
        in   
      -- Called when we need a "NoOp"
        ( clickHandler x y model, Cmd.none)

      Reset -> 
      -- Called to reset player coins
        ({ model | coins = 0}, Cmd.none ) 

      AddCoins val ->
      -- Called to add or subtract a given value from the player's total money
        ( { model | coins = model.coins + val }, Cmd.none)
      
      ChangePage pg ->
        ( { model | page = pg }, Cmd.none)

      NoOp ->   
      -- Called when we need a "NoOp"
        ( model, Cmd.none )

{-
  Takes a click and model, returns updated model (called by update on a click event)

  Args:
    x, y -> Click coordinates
    model -> Current Model

  Returns ->
    new model 
-}
clickHandler : Float -> Float -> Model -> Model 
clickHandler x y model =
  -- First, check if anything was clicked
  case (clickedAnyButton x y model.page model.buttons) of 
    Nothing -> model
    Just btype -> 
      let
        _ = Debug.log "Clicked Button" btype
      in
        -- If something was clicked, case the type of button
        case btype of 
          Plot ptype -> 
            case P.plotClicked model.plants ptype model.coins of
              Nothing -> model
              Just (plants, profit) -> {model | plants = plants, coins = model.coins + profit}
          Upgrade ptype ->
            case P.upgradeClicked model.plants ptype model.coins of
               Nothing -> model
               Just (plants, profit) -> 
                let
                  _ = Debug.log "Status" ((P.getPlant ptype plants), profit)
                in
                  {model | plants = plants, coins = model.coins + profit}


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
          [ text ("Time: " ++ VH.frameToTime model.frame)]--("Frame: " ++ String.fromFloat model.frame)]
    ]

{-
  Determines which game page should be displayed
-}
gameView : Model -> Html Msg
gameView model = 
  case model.page of
    Farm -> VH.displayFarm model
    Store -> VH.displayFarm model