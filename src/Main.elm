module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Dimensions =
    { width : Int, height : Int }


type alias Model =
    { x : Int
    , y : Int
    , grid : Dimensions
    , cellSize : Int
    , gridMargin : { top : Int, right : Int, bottom : Int, left : Int }
    , screen : Maybe Dimensions
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { x = 0
      , y = 0
      , grid = { width = 5, height = 5 }
      , cellSize = 200
      , gridMargin = { top = 20, right = 0, bottom = 80, left = 0 }
      , screen = Nothing
      }
    , Task.perform (\{ viewport } -> ScreenSize (round viewport.width) (round viewport.height)) getViewport
    )



-- UPDATE


type Msg
    = TurnLeft
    | TurnRight
    | MoveForward
    | Other
    | ScreenSize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TurnLeft ->
            ( model, Cmd.none )

        -- ( { model | facing = cycleNextItem (List.reverse clockwiseDirections) model.facing }, Cmd.none )
        TurnRight ->
            ( model, Cmd.none )

        -- ( { model | facing = cycleNextItem clockwiseDirections model.facing }, Cmd.none )
        MoveForward ->
            ( model, Cmd.none )

        -- ( move model, Cmd.none )
        ScreenSize w h ->
            ( { model | screen = Just { width = w, height = h } }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown keyDecoder
        , onResize ScreenSize
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            TurnLeft

        "ArrowRight" ->
            TurnRight

        "ArrowUp" ->
            MoveForward

        _ ->
            Other



-- VIEW


clearScreen : Float -> Float -> Renderable
clearScreen width height =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


view : Model -> Html Msg
view { screen } =
    case screen of
        Just dimensions ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                [ Canvas.toHtml
                    ( dimensions.width, dimensions.height )
                    []
                    [ clearScreen (toFloat dimensions.width) (toFloat dimensions.height)
                    ]
                ]

        Nothing ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                []
