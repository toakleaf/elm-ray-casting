module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
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


type alias Polar =
    { dist : Float, rot : Float }


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
    , playerRadSize : Float
    , movement : Polar
    , velocity : Polar
    , grid : Grid
    , gridDimensions : Dimensions
    , tileSize : Int
    , canvasSize : Maybe Dimensions
    , screenSize : Maybe Dimensions
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playerPos = { x = 0, y = 0, angle = 0 }
      , playerRadSize = 10
      , movement = { dist = 0, rot = 0 }
      , velocity = { dist = 3, rot = 3 }
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
    = Frame Float
    | TurnLeft
    | TurnRight
    | MoveForward
    | MoveBackward
    | CancelLeft
    | CancelRight
    | CancelForward
    | CancelBackward
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


hasCollision : Model -> Position -> Bool
hasCollision model pos =
    let
        rad =
            model.playerRadSize

        subjectCorners =
            [ ( floor (pos.x - rad), floor (pos.y - rad) ) -- left top
            , ( ceiling (pos.x + rad), floor (pos.y - rad) ) -- right top
            , ( ceiling (pos.x + rad), ceiling (pos.y + rad) ) -- right bottom
            , ( floor (pos.x - rad), ceiling (pos.y + rad) ) -- left bottom
            ]

        check : ( Int, Int ) -> Bool
        check tup =
            let
                x =
                    Tuple.first tup // model.tileSize

                y =
                    Tuple.second tup // model.tileSize

                cell =
                    List.drop y model.grid |> List.take 1 |> List.concat |> List.drop x

                item =
                    List.head cell
            in
            item /= Just 0
    in
    List.map check subjectCorners |> List.any (\a -> a == True)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { x, y, angle } =
            model.playerPos

        ( dx, dy ) =
            fromPolar ( model.movement.dist, degrees (angle + model.movement.rot) )

        { dist, rot } =
            model.movement
    in
    case msg of
        Frame _ ->
            let
                collisions =
                    hasCollision model { x = x + dx, y = y + dy, angle = angle }
            in
            ( if collisions /= True then
                { model
                    | playerPos = { x = x + dx, y = y + dy, angle = angle + model.movement.rot }
                }

              else if dy > 0 && dx < 0 then
                -- up left
                { model
                    | playerPos = { x = x + dx, y = y + dy, angle = angle + model.movement.rot }
                }

              else if dy > 0 && dx >= 0 then
                -- up right
                model

              else if dy <= 0 && dx < 0 then
                -- down left
                model

              else
                -- down right
                model
            , Cmd.none
            )

        TurnLeft ->
            ( { model | movement = { dist = dist, rot = model.velocity.rot * -1 } }, Cmd.none )

        TurnRight ->
            ( { model | movement = { dist = dist, rot = model.velocity.rot } }, Cmd.none )

        MoveForward ->
            ( { model | movement = { dist = model.velocity.dist, rot = rot } }, Cmd.none )

        MoveBackward ->
            ( { model | movement = { dist = model.velocity.dist * -1, rot = rot } }, Cmd.none )

        CancelLeft ->
            ( { model | movement = { dist = dist, rot = rot + model.velocity.rot } }, Cmd.none )

        CancelRight ->
            ( { model | movement = { dist = dist, rot = rot - model.velocity.rot } }, Cmd.none )

        CancelForward ->
            ( { model | movement = { dist = dist - model.velocity.dist, rot = rot } }, Cmd.none )

        CancelBackward ->
            ( { model | movement = { dist = dist + model.velocity.dist, rot = rot } }, Cmd.none )

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
                , playerRadSize = toFloat size / 3
              }
            , Cmd.none
            )

        Other ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown keyDecoderDown
        , onKeyUp keyDecoderUp
        , onResize ScreenSize
        , onAnimationFrameDelta Frame
        ]


keyDecoderDown : Decode.Decoder Msg
keyDecoderDown =
    Decode.map toMovement (Decode.field "key" Decode.string)


keyDecoderUp : Decode.Decoder Msg
keyDecoderUp =
    Decode.map clearMovement (Decode.field "key" Decode.string)


toMovement : String -> Msg
toMovement string =
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


clearMovement : String -> Msg
clearMovement string =
    case string of
        "ArrowLeft" ->
            CancelLeft

        "ArrowRight" ->
            CancelRight

        "ArrowUp" ->
            CancelForward

        "ArrowDown" ->
            CancelBackward

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
    [ shapes [ fill Color.blue ] [ circle ( model.playerPos.x, model.playerPos.y ) model.playerRadSize ]
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
