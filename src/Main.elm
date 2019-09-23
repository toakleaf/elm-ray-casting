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


type alias Position =
    { x : Int
    , y : Int
    , angle : Float
    }


type alias Grid =
    List (List Int)


tileMap : Grid
tileMap =
    [ [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1 ]
    , [ 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1 ]
    , [ 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
    , [ 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
    , [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
    ]


getGridDimensions : Grid -> Dimensions
getGridDimensions grid =
    { width = List.head grid |> Maybe.withDefault [] |> List.length, height = List.length grid }


type alias Model =
    { pos : Position
    , grid : Grid
    , gridSize : Dimensions
    , tileSize : Int
    , canvasSize : Dimensions
    , screenSize : Maybe Dimensions
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pos = { x = 0, y = 0, angle = 0 }
      , grid = tileMap
      , gridSize = getGridDimensions tileMap
      , tileSize = 32
      , canvasSize = { width = (getGridDimensions tileMap).width * 32, height = (getGridDimensions tileMap).height * 32 }
      , screenSize = Nothing
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

        TurnRight ->
            ( model, Cmd.none )

        MoveForward ->
            ( model, Cmd.none )

        ScreenSize w h ->
            ( { model | screenSize = Just { width = w, height = h } }
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


makeTile : Model -> Int -> Int -> Renderable
makeTile model i tileType =
    let
        fillColor =
            case tileType of
                0 ->
                    Color.white

                1 ->
                    Color.red

                _ ->
                    Color.blue

        length =
            List.concat model.grid |> List.length

        row =
            i // model.gridSize.width

        col =
            i - row * model.gridSize.width
    in
    shapes [ fill fillColor, stroke Color.black ] [ rect ( toFloat (col * model.tileSize), toFloat (row * model.tileSize) ) (toFloat model.tileSize) (toFloat model.tileSize) ]


renderMap : Model -> List Renderable
renderMap model =
    let
        partialMakeTile =
            makeTile model
    in
    List.concat model.grid |> List.indexedMap partialMakeTile


view : Model -> Html Msg
view model =
    case model.screenSize of
        Just dimensions ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                [ Canvas.toHtml
                    ( dimensions.width, dimensions.height )
                    []
                    (clearScreen (toFloat dimensions.width) (toFloat dimensions.height) :: renderMap model)
                ]

        Nothing ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                []
