module Main exposing (..)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Svg exposing (Attribute, Svg, circle, defs, g, pattern, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, id, r, rotate, rx, ry, stroke, transform, viewBox, width, x, y)
import Svg.Lazy exposing (lazy)
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


type alias Model =
    { x : Int
    , y : Int
    , facing : Direction
    , moveDistance : Int
    , grid : { width : Int, height : Int }
    , cellSize : Int
    , gridMargin : { top : Int, right : Int, bottom : Int, left : Int }
    , screen : ( Int, Int )
    }


type Direction
    = North
    | East
    | South
    | West


init : () -> ( Model, Cmd Msg )
init _ =
    ( { x = 0
      , y = 0
      , facing = East
      , moveDistance = 1
      , grid = { width = 5, height = 5 }
      , cellSize = 200
      , gridMargin = { top = 20, right = 0, bottom = 80, left = 0 }
      , screen = ( 800, 600 )
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


clockwiseDirections =
    [ North, East, South, West ]


cycleNextItem : List Direction -> Direction -> Direction
cycleNextItem list dir =
    case list of
        [] ->
            dir

        x :: xs ->
            if x == dir then
                xs |> List.head |> Maybe.withDefault dir

            else
                cycleNextItem (xs ++ [ x ]) dir


move : Model -> Model
move model =
    case model.facing of
        North ->
            if model.y - model.moveDistance <= 0 then
                { model | y = 0 }

            else
                { model | y = model.y - model.moveDistance }

        South ->
            if model.y + model.moveDistance >= model.grid.height then
                { model | y = model.grid.height - 1 }

            else
                { model | y = model.y + model.moveDistance }

        East ->
            if model.x + model.moveDistance >= model.grid.width then
                { model | x = model.grid.width - 1 }

            else
                { model | x = model.x + model.moveDistance }

        West ->
            if model.x - model.moveDistance <= 0 then
                { model | x = 0 }

            else
                { model | x = model.x - model.moveDistance }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TurnLeft ->
            ( { model | facing = cycleNextItem (List.reverse clockwiseDirections) model.facing }, Cmd.none )

        TurnRight ->
            ( { model | facing = cycleNextItem clockwiseDirections model.facing }, Cmd.none )

        MoveForward ->
            ( move model, Cmd.none )

        ScreenSize width height ->
            ( { model | screen = ( width, height ) }
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


getPercent : Int -> Int -> String
getPercent part total =
    String.fromFloat (toFloat part / toFloat total * 100) ++ "%"


getRobotRotation : Model -> String
getRobotRotation model =
    let
        degrees : Direction -> String
        degrees dir =
            case dir of
                East ->
                    "0"

                South ->
                    "90"

                West ->
                    "180"

                North ->
                    "-90"
    in
    "rotate(" ++ degrees model.facing ++ " " ++ String.fromInt (model.cellSize // 2) ++ " " ++ String.fromInt (model.cellSize // 2) ++ " )"


drawRobot : Model -> Svg msg
drawRobot model =
    svg
        [ x (String.fromInt (model.cellSize * model.x))
        , y (String.fromInt (model.cellSize * model.y))
        ]
        [ g [ transform (getRobotRotation model) ]
            [ rect
                [ x (String.fromInt (model.cellSize // 3))
                , y (String.fromInt (model.cellSize // 8))
                , width (String.fromInt (model.cellSize // 3))
                , height (String.fromInt (model.cellSize // 8))
                , stroke "black"
                , fill "white"
                , rx "0.5%"
                , ry "0.5%"
                ]
                []
            , circle
                [ cx (String.fromInt (model.cellSize // 2))
                , cy (String.fromInt (model.cellSize // 2))
                , r (String.fromInt (model.cellSize // 4))
                , stroke "white"
                , fill "blue"
                ]
                []
            , circle
                [ cx (String.fromInt (model.cellSize // 2))
                , cy (String.fromInt (model.cellSize // 2))
                , r (String.fromInt (model.cellSize // 6))
                , stroke "black"
                , fill "white"
                ]
                []
            , circle
                [ cx (String.fromInt (model.cellSize // 2 + model.cellSize // 6))
                , cy (String.fromInt (model.cellSize // 2))
                , r (String.fromInt (model.cellSize // 20))
                , stroke "white"
                , fill "black"
                ]
                []
            , rect
                [ x (String.fromInt (model.cellSize // 3))
                , y (String.fromInt ((model.cellSize // 8) * 6))
                , width (String.fromInt (model.cellSize // 3))
                , height (String.fromInt (model.cellSize // 8))
                , stroke "black"
                , fill "white"
                , rx "0.5%"
                , ry "0.5%"
                ]
                []
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ viewBox ("0 0" ++ " " ++ String.fromInt (model.cellSize * model.grid.width) ++ " " ++ String.fromInt (model.cellSize * model.grid.height))
            , width (String.fromInt (Tuple.first model.screen - model.gridMargin.top))
            , height (String.fromInt (Tuple.second model.screen - model.gridMargin.bottom))
            ]
            [ defs []
                [ pattern
                    [ id "Pattern"
                    , x "0"
                    , y "0"
                    , width (getPercent 1 model.grid.width)
                    , height (getPercent 1 model.grid.height)
                    ]
                    [ rect
                        [ x "0"
                        , y "0"
                        , width (getPercent 1 model.grid.width)
                        , height (getPercent 1 model.grid.height)
                        , rx "1%"
                        , ry "1%"
                        , fill "black"
                        , stroke "white"
                        ]
                        []
                    ]
                ]
            , rect
                [ fill "url(#Pattern)"
                , width "100%"
                , height "100%"
                ]
                []
            , lazy drawRobot model
            ]
        , div [ style "textAlign" "center", style "fontSize" "150%" ]
            [ text "Use arrow keys to rotate robot left/right or move forward."
            ]
        ]
