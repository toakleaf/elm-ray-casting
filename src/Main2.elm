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


type alias Ray =
    { angle : Float
    , hitX : ( Int, Int )
    , hitY : ( Int, Int )
    , len : Float
    , wasHitVert : Bool
    }


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
    , fov : Float
    , numRays : Int
    , rays : List Ray
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
      , fov = 60
      , numRays = 30
      , rays = []
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


remainderByFloat : Float -> Float -> Float
remainderByFloat denom numer =
    numer - denom * toFloat (floor (numer / denom))


normalizeRad : Float -> Float
normalizeRad ang =
    let
        a =
            remainderByFloat (2 * pi) ang
    in
    if a < 0 then
        a + 2 * pi

    else
        a


normalizeDeg : Float -> Float
normalizeDeg ang =
    let
        a =
            remainderByFloat 360 ang
    in
    if a < 0 then
        a + 360

    else
        a


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


tileAtCoords : Grid -> ( Int, Int ) -> Maybe Int
tileAtCoords grid ( x, y ) =
    List.drop y grid |> List.take 1 |> List.concat |> List.drop x |> List.head


tileAtPos : Model -> ( Int, Int ) -> Maybe Int
tileAtPos model tup =
    tileAtCoords model.grid ( Tuple.first tup // model.tileSize, Tuple.second tup // model.tileSize )


hasCollision : Model -> List ( Int, Int ) -> Bool
hasCollision model tups =
    List.map (\t -> tileAtPos model t) tups |> List.any (\a -> a /= Just 0)


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
                rad =
                    model.playerRadSize

                playerCorners : Float -> Float -> List ( Int, Int )
                playerCorners posX posY =
                    [ ( floor (posX - rad), floor (posY - rad) ) -- left top
                    , ( ceiling (posX + rad), floor (posY - rad) ) -- right top
                    , ( ceiling (posX + rad), ceiling (posY + rad) ) -- right bottom
                    , ( floor (posX - rad), ceiling (posY + rad) ) -- left bottom
                    ]

                collisionX =
                    hasCollision model (playerCorners (x + dx) y)

                collisionY =
                    hasCollision model (playerCorners x (y + dy))

                -- test =
                --     horizontalIntercept model model.playerPos Nothing
                -- test2 =
                --     ( Debug.log "test" test, Debug.log "size" model.tileSize )
            in
            ( if collisionX && collisionY then
                { model
                    | playerPos = { x = x, y = y, angle = normalizeDeg (angle + model.movement.rot) }
                }

              else if collisionX then
                { model
                    | playerPos = { x = x, y = y + dy, angle = normalizeDeg (angle + model.movement.rot) }
                }

              else if collisionY then
                { model
                    | playerPos = { x = x + dx, y = y, angle = normalizeDeg (angle + model.movement.rot) }
                }

              else
                { model
                    | playerPos = { x = x + dx, y = y + dy, angle = normalizeDeg (angle + model.movement.rot) }
                }
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
            let
                test =
                    horizontalIntercept model model.playerPos Nothing

                test2 =
                    Debug.log "RESULT" test
            in
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


horizontalIntercept : Model -> Position -> Maybe ( Float, Float ) -> Maybe ( Float, Float )
horizontalIntercept model { x, y, angle } step =
    let
        size =
            Debug.log "SSSSSSSSSSSTARTTTTTTTTTTTTTTTT" (toFloat model.tileSize)

        row =
            if angle < 180 then
                floor (y / size)

            else
                -- slight offset so we are colliding with propper row as 0 is first index in next row
                floor ((y - 1) / size)

        ( top, bottom ) =
            Debug.log "top/bot" ( toFloat row * size, toFloat row * size + size )

        ( newX, newY ) =
            case step of
                Nothing ->
                    if angle > 0 && angle <= 180 then
                        -- values account for +/- changes w/ quadrants
                        Debug.log "DOWN" ( x + (bottom - y) / tan (degrees angle), bottom )

                    else
                        Debug.log "UP" ( x - (y - top) / tan (degrees angle), top )

                Just tup ->
                    -- if angle > 0 && angle <= 180 then
                    ( Tuple.first tup + x, Tuple.second tup + y )

        -- else
        --     ( x - Tuple.first tup, y - Tuple.second tup )
        ( newCol, newRow ) =
            if angle < 180 then
                Debug.log "NEWWWW COOOOL" ( floor (Debug.log "xxxxxxxxxxxxxx" newX / size), floor (newY / size) )

            else
                ( floor (newX / size), floor (newY / size) - 1 )

        nextStep =
            if abs (round (Debug.log "**********" newY - Debug.log "%%%%%%%%%%%%%%" y)) == round size then
                Just ( newX - x, newY - y )
                -- else if abs (round (y)) == 0 then
                --     Just ( newX - x, -size )
                --     -- Just ( x - newX, y - newY )
                -- else if abs (round (y - newY)) == round size then
                --     Just ( newX - x, y - newY )

            else
                Nothing

        test =
            Debug.log "NextStep" nextStep

        test6 =
            Debug.log "Step" step

        test2 =
            tileAtCoords model.grid (Debug.log "!!!! THE TILE !!!!" ( newCol, newRow ))

        test3 =
            Debug.log "!!!! CHECK TILE !!!!" test2
    in
    if newCol < 0 || newCol >= model.gridDimensions.width || newRow < 0 || newRow >= model.gridDimensions.height || angle == 0 then
        Debug.log "OUT OF BOUNDS" Nothing

    else if tileAtCoords model.grid ( newCol, newRow ) /= Just 0 then
        Debug.log "JUST X, Y" Just ( newX, newY )

    else
        Debug.log "REPEAT" horizontalIntercept model { x = newX, y = newY, angle = angle } nextStep


verticalIntercept : Model -> Position -> Maybe ( Float, Float ) -> Maybe ( Float, Float )
verticalIntercept model { x, y, angle } step =
    let
        size =
            toFloat model.tileSize

        ( col, row ) =
            ( floor (x / size), floor (y / size) )

        ( left, right ) =
            ( toFloat col * size, toFloat col * size + size )

        ( newX, newY ) =
            case step of
                Nothing ->
                    if angle > 90 && angle <= 270 then
                        -- values account for +/- changes w/ quadrants
                        ( left, y - (x - left) * tan (degrees angle) )

                    else
                        ( right, y + (right - x) * tan (degrees angle) )

                Just tup ->
                    ( Tuple.first tup + x, Tuple.second tup + y )

        nextStep =
            if abs (round (newX - x)) == round size then
                Just ( newX - x, newY - y )

            else
                Nothing
    in
    if col < 0 || col >= model.gridDimensions.width || row < 0 || row >= model.gridDimensions.height then
        Nothing

    else if tileAtPos model ( floor (newX / size), floor (newY / size) ) /= Just 0 then
        Just ( x, y )

    else
        verticalIntercept model { x = newX, y = newY, angle = angle } nextStep



-- castRay : Model -> Position -> Ray
-- castRay model origin =
--     let
--     in
--     { angle = origin.angle
--     , hitX = ( Int, Int )
--     , hitY = ( Int, Int )
--     , len = Float
--     , wasHitVert = Bool
--     }


renderRay : Model -> Float -> Float -> Shape
renderRay model len angle =
    let
        ( dx, dy ) =
            fromPolar ( len, degrees angle )
    in
    path ( model.playerPos.x, model.playerPos.y )
        [ lineTo
            ( model.playerPos.x + dx
            , model.playerPos.y + dy
            )
        ]


renderPlayer : Model -> List Renderable
renderPlayer model =
    let
        lineLength =
            100

        ( dx, dy ) =
            fromPolar ( lineLength, degrees model.playerPos.angle )

        stepSize =
            model.fov / toFloat model.numRays

        li =
            List.range 0 model.numRays |> List.map (\n -> model.fov / 2 + model.playerPos.angle - stepSize * toFloat n)
    in
    [ shapes
        [ stroke Color.lightBlue
        , lineWidth 1
        ]
        (List.map (\n -> renderRay model lineLength n) li)
    , shapes [ fill Color.blue ] [ circle ( model.playerPos.x, model.playerPos.y ) model.playerRadSize ]
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
