module Game exposing (..)

import Browser
import Browser.Events as BEvents
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Html


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BEvents.onClick (Decode.succeed Increment)
        , BEvents.onKeyDown
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



-- MODEL


type alias Model =
    { count : Int }


initModel : Model
initModel =
    { count = 0 }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Reset
    | Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update: " msg
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( initModel, Cmd.none )

        Increment ->
            ( { count = 1 + model.count }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.text "Welcome to our game! Click to increment, press esc. to decrement.",
          Html.br
            [] [],
          Html.text ("Count: " ++ String.fromInt model.count)
        ]
