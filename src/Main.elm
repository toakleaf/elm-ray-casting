module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
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



-- MODEL --


type alias Dimensions =
    { width : Int, height : Int }


type alias Position =
    { x : Float
    , y : Float
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
    { playerPos : Position
    , playerVel : Float
    , playerRotVel : Float
    , grid : Grid
    , gridDimensions : Dimensions
    , tileSize : Int
    , canvasSize : Maybe Dimensions
    , screenSize : Maybe Dimensions
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playerPos = { x = 0, y = 0, angle = 0 }
      , playerVel = 5
      , playerRotVel = 5
      , grid = tileMap
      , gridDimensions = getGridDimensions tileMap
      , tileSize = 32
      , canvasSize = Nothing
      , screenSize = Nothing
      }
    , Task.perform (\{ viewport } -> ScreenSize (round viewport.width) (round viewport.height)) getViewport
    )



-- UPDATE --


type Msg
    = TurnLeft
    | TurnRight
    | MoveForward
    | MoveBackward
    | Other
    | ScreenSize Int Int


indexOfInt : Int -> List Int -> Int
indexOfInt num list =
    let
        helper : List Int -> Int -> Int -> Int
        helper li elem offset =
            case li of
                [] ->
                    -1

                x :: xs ->
                    if x == elem then
                        offset

                    else
                        helper xs elem (offset + 1)
    in
    helper list num 0


get2DIndiciesFrom1DList : Int -> Int -> ( Int, Int )
get2DIndiciesFrom1DList width index =
    let
        row =
            index // width

        col =
            index - row * width
    in
    if width < 0 || index < 0 then
        ( -1, -1 )

    else
        ( col, row )


indiciesOfGrid : Int -> List (List Int) -> ( Int, Int )
indiciesOfGrid num grid =
    let
        i =
            List.concat grid |> indexOfInt num

        width =
            List.take 1 grid |> List.concat |> List.length
    in
    if i < 0 then
        ( -1, -1 )

    else
        get2DIndiciesFrom1DList width i


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( dx, dy ) =
            fromPolar ( model.playerVel, degrees model.playerPos.angle )
    in
    case msg of
        TurnLeft ->
            ( { model | playerPos = { x = model.playerPos.x, y = model.playerPos.y, angle = model.playerPos.angle - model.playerRotVel } }, Cmd.none )

        TurnRight ->
            ( { model | playerPos = { x = model.playerPos.x, y = model.playerPos.y, angle = model.playerPos.angle + model.playerRotVel } }, Cmd.none )

        MoveForward ->
            ( { model | playerPos = { x = model.playerPos.x + dx, y = model.playerPos.y + dy, angle = model.playerPos.angle } }, Cmd.none )

        MoveBackward ->
            ( { model | playerPos = { x = model.playerPos.x - dx, y = model.playerPos.y - dy, angle = model.playerPos.angle } }, Cmd.none )

        ScreenSize w h ->
            let
                wide =
                    w // model.gridDimensions.width

                tall =
                    h // model.gridDimensions.height

                size =
                    if wide * model.gridDimensions.height < h then
                        wide

                    else
                        tall

                halfSize =
                    size // 2

                ( xZeroed, yZeroed ) =
                    indiciesOfGrid 0 model.grid
            in
            ( { model
                | screenSize = Just { width = w, height = h }
                , canvasSize = Just { width = (getGridDimensions tileMap).width * size, height = (getGridDimensions tileMap).height * size }
                , tileSize = size
                , playerPos =
                    if model.playerPos == { x = 0, y = 0, angle = 0 } then
                        { x = toFloat (xZeroed * size + halfSize), y = toFloat (yZeroed * size + halfSize), angle = 0 }

                    else
                        model.playerPos
              }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS --


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

        "ArrowDown" ->
            MoveBackward

        _ ->
            Other



-- VIEW --


clearScreen : Float -> Float -> Renderable
clearScreen width height =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


makeTile : Float -> Position -> Model -> Int -> Int -> Renderable
makeTile scale offset model index tileType =
    let
        fillColor =
            case tileType of
                0 ->
                    Color.white

                1 ->
                    Color.red

                _ ->
                    Color.blue

        ( x, y ) =
            get2DIndiciesFrom1DList model.gridDimensions.width index
    in
    shapes
        [ fill fillColor, stroke Color.black ]
        [ rect
            ( scale * toFloat (x * model.tileSize) + offset.x
            , scale * toFloat (y * model.tileSize) + offset.y
            )
            (scale * toFloat model.tileSize)
            (scale * toFloat model.tileSize)
        ]


renderMap : Model -> List Renderable
renderMap model =
    let
        partialMakeTile =
            makeTile 1 { x = 0, y = 0, angle = 0 } model
    in
    List.concat model.grid |> List.indexedMap partialMakeTile


renderPlayer : Model -> List Renderable
renderPlayer model =
    let
        lineLength =
            100

        ( dx, dy ) =
            fromPolar ( lineLength, degrees model.playerPos.angle )
    in
    [ shapes [ fill Color.blue ] [ circle ( model.playerPos.x, model.playerPos.y ) (toFloat model.tileSize / 3) ]
    , shapes
        [ stroke Color.blue
        , lineWidth 5
        ]
        [ path ( model.playerPos.x, model.playerPos.y )
            [ lineTo
                ( model.playerPos.x + dx
                , model.playerPos.y + dy
                )
            ]
        ]
    ]


view : Model -> Html Msg
view model =
    case model.canvasSize of
        Just dimensions ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "background" "black"
                ]
                [ Canvas.toHtml
                    ( dimensions.width, dimensions.height )
                    []
                    (clearScreen (toFloat dimensions.width) (toFloat dimensions.height)
                        :: renderMap model
                        ++ renderPlayer model
                    )
                ]

        Nothing ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                []
