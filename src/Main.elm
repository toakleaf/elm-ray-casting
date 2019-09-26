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
import Html.Lazy exposing (lazy, lazy4)
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
    , hit : ( Float, Float )
    , len : Float
    , hitVert : Bool
    }


tileMap : Grid
tileMap =
    [ [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1 ]
    , [ 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1 ]
    , [ 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
    , [ 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1 ]
    , [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]
    , [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
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
    , grid : Grid
    , gridDimensions : Dimensions
    , tileSize : Int
    , mapPos : Position
    , mapScale : Float
    , wallScale : Float
    , wallHeight : Int
    , sliceWidth : Float
    , canvasSize : Maybe Dimensions
    , screenSize : Maybe Dimensions
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playerPos = { x = 0, y = 0, angle = 0 }
      , playerRadSize = 10
      , movement = { dist = 0, rot = 0 }
      , velocity = { dist = 3, rot = 2 }
      , fov = 60
      , numRays = 2
      , grid = tileMap
      , gridDimensions = getGridDimensions tileMap
      , tileSize = 32
      , mapPos = { x = 0, y = 0, angle = 0 }
      , mapScale = 0.25
      , wallScale = 0.6
      , wallHeight = 32
      , sliceWidth = 2
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
        { dist, rot } =
            model.movement
    in
    case msg of
        Frame _ ->
            let
                { x, y, angle } =
                    model.playerPos

                ( dx, dy ) =
                    fromPolar ( dist, degrees (angle + rot) )

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
            in
            ( if dist == 0 && rot == 0 then
                model

              else if collisionX && collisionY then
                { model
                    | playerPos = { x = x, y = y, angle = normalizeDeg (angle + rot) }
                }

              else if collisionX then
                { model
                    | playerPos = { x = x, y = y + dy, angle = normalizeDeg (angle + rot) }
                }

              else if collisionY then
                { model
                    | playerPos = { x = x + dx, y = y, angle = normalizeDeg (angle + rot) }
                }

              else
                { model
                    | playerPos = { x = x + dx, y = y + dy, angle = normalizeDeg (angle + rot) }
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
                ( wide, tall ) =
                    ( w // model.gridDimensions.width, h // model.gridDimensions.height )

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
                , numRays = floor (toFloat ((getGridDimensions tileMap).width * size) / model.sliceWidth)
                , wallHeight = floor (toFloat ((getGridDimensions tileMap).width * size) * model.wallScale)
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


horizontalIntercept : Model -> Position -> Maybe ( Float, Float ) -> Maybe ( Float, Float )
horizontalIntercept model { x, y, angle } step =
    let
        size =
            toFloat model.tileSize

        row =
            if angle > 0 && angle <= 180 then
                floor (y / size)

            else
                -- slight offset so we are colliding with propper row as 0 is first index in next row
                floor ((y - 1) / size)

        ( top, bottom ) =
            ( toFloat row * size, toFloat row * size + size )

        ( newX, newY ) =
            case step of
                Nothing ->
                    if angle > 0 && angle <= 180 then
                        -- values account for +/- changes w/ quadrants
                        ( x + (bottom - y) / tan (degrees angle), bottom )

                    else
                        ( x - (y - top) / tan (degrees angle), top )

                Just tup ->
                    ( Tuple.first tup + x, Tuple.second tup + y )

        ( newCol, newRow ) =
            if angle > 0 && angle <= 180 then
                ( floor (newX / size), floor (newY / size) )

            else
                -- slight offset so we are colliding with propper row as 0 is first index in next row
                ( floor (newX / size), floor ((newY - 1) / size) )

        nextStep =
            if abs (round (newY - y)) == round size then
                Just ( newX - x, newY - y )

            else
                Nothing
    in
    if newCol < 0 || newCol >= model.gridDimensions.width || newRow < 0 || newRow >= model.gridDimensions.height || angle == 0 || angle == 180 then
        Nothing

    else if tileAtCoords model.grid ( newCol, newRow ) /= Just 0 then
        Just ( newX, newY )

    else
        horizontalIntercept model { x = newX, y = newY, angle = angle } nextStep


verticalIntercept : Model -> Position -> Maybe ( Float, Float ) -> Maybe ( Float, Float )
verticalIntercept model { x, y, angle } step =
    let
        size =
            toFloat model.tileSize

        col =
            if angle > 90 && angle <= 270 then
                -- slight offset so we are colliding with propper row as 0 is first index in next row
                floor ((x - 1) / size)

            else
                floor (x / size)

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

        ( newCol, newRow ) =
            if angle > 90 && angle <= 270 then
                -- slight offset so we are colliding with propper row as 0 is first index in next row
                ( floor ((newX - 1) / size), floor (newY / size) )

            else
                ( floor (newX / size), floor (newY / size) )

        nextStep =
            if abs (round (newX - x)) == round size then
                Just ( newX - x, newY - y )

            else
                Nothing
    in
    if newCol < 0 || newCol >= model.gridDimensions.width || newRow < 0 || newRow >= model.gridDimensions.height || angle == 90 || angle == 270 then
        Nothing

    else if tileAtCoords model.grid ( newCol, newRow ) /= Just 0 then
        Just ( newX, newY )

    else
        verticalIntercept model { x = newX, y = newY, angle = angle } nextStep


getHypotenuse : ( Float, Float ) -> ( Float, Float ) -> Float
getHypotenuse a b =
    let
        width =
            abs (Tuple.first b - Tuple.first a)

        height =
            abs (Tuple.second b - Tuple.second a)
    in
    sqrt (width ^ 2 + height ^ 2)


castRay : Model -> Position -> Ray
castRay model origin =
    let
        hitX =
            horizontalIntercept model origin Nothing

        hitY =
            verticalIntercept model origin Nothing

        hor =
            case hitX of
                Nothing ->
                    -- 1/0 is infinity in elm
                    ( 1 / 0, ( 0, 0 ) )

                Just tup ->
                    ( getHypotenuse ( origin.x, origin.y ) tup, tup )

        vert =
            case hitY of
                Nothing ->
                    -- 1/0 is infinity in elm
                    ( 1 / 0, ( 0, 0 ) )

                Just tup ->
                    ( getHypotenuse ( origin.x, origin.y ) tup, tup )

        isVert =
            Tuple.first vert < Tuple.first hor
    in
    { angle = origin.angle
    , hit =
        if isVert then
            Tuple.second vert

        else
            Tuple.second hor
    , len = min (Tuple.first hor) (Tuple.first vert)
    , hitVert = isVert
    }


renderRay : Model -> Ray -> Shape
renderRay model { angle, hit, len, hitVert } =
    let
        ( dx, dy ) =
            fromPolar ( len, degrees angle )

        ( x, y ) =
            ( model.playerPos.x * model.mapScale + model.mapPos.x, model.playerPos.y * model.mapScale + model.mapPos.y )

        ( x2, y2 ) =
            ( (model.playerPos.x + dx) * model.mapScale + model.mapPos.x, (model.playerPos.y + dy) * model.mapScale + model.mapPos.y )
    in
    path ( x, y )
        [ lineTo ( x2, y2 ) ]


getRayList : Model -> List Ray
getRayList model =
    let
        pos =
            model.playerPos

        stepSize =
            model.fov / toFloat model.numRays

        angList =
            List.range 0 model.numRays |> List.map (\n -> model.fov / 2 + pos.angle - stepSize * toFloat n) |> List.map normalizeDeg
    in
    List.map (\ang -> castRay model { pos | angle = ang }) angList


renderPlayer : Model -> List Ray -> List Renderable
renderPlayer model rayList =
    let
        ( x, y ) =
            ( model.playerPos.x * model.mapScale + model.mapPos.x, model.playerPos.y * model.mapScale + model.mapPos.y )
    in
    [ shapes
        [ stroke Color.lightBlue
        , lineWidth 1
        ]
        (List.map (\ray -> renderRay model ray) rayList)
    , shapes [ fill (Color.rgb255 0 102 0) ] [ circle ( x, y ) (model.playerRadSize * model.mapScale) ]
    ]


render3D : Model -> List Ray -> List Renderable
render3D model rayList =
    let
        slice : Int -> Ray -> List Renderable
        slice index { len, hitVert, angle } =
            let
                ( canvasW, canvasH ) =
                    case model.canvasSize of
                        Nothing ->
                            ( 0, 0 )

                        Just size ->
                            ( toFloat size.width, toFloat size.height )

                distToProjPlane =
                    (canvasW / 2) / tan (model.fov / 2)

                unskewedRayLength =
                    len * cos (degrees angle - degrees model.playerPos.angle)

                height =
                    toFloat model.wallHeight / unskewedRayLength * distToProjPlane

                alphaWall =
                    170 / unskewedRayLength

                wallColor =
                    if hitVert then
                        Color.rgba 0.969 0.949 0.925 alphaWall

                    else
                        Color.rgba 0.925 0.871 0.816 alphaWall

                floorColor =
                    Color.rgba 0.2 0.2 0.2 alphaWall

                ceilingColor =
                    Color.rgba 0.859 0.941 0.976 1
            in
            [ shapes [ fill floorColor ]
                [ rect
                    ( toFloat index * model.sliceWidth
                    , canvasH / 2 - height / 2
                    )
                    model.sliceWidth
                    ((canvasH - height) / 2)
                ]

            -- , shapes [ fill ceilingColor ]
            --     [ rect
            --         ( toFloat index * model.sliceWidth
            --         , 0
            --         )
            --         model.sliceWidth
            --         ((canvasH + height) / 2)
            -- ]
            , shapes [ fill wallColor ]
                [ rect
                    ( toFloat index * model.sliceWidth
                    , canvasH / 2 - height / 2
                    )
                    model.sliceWidth
                    height
                ]
            ]
    in
    List.indexedMap slice (List.reverse rayList) |> List.concat


makeTile : Float -> Position -> Model -> Int -> Int -> Renderable
makeTile scale offset model index tileType =
    let
        fillColor =
            case tileType of
                0 ->
                    Color.white

                1 ->
                    Color.darkBlue

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
            makeTile model.mapScale model.mapPos model
    in
    List.concat model.grid |> List.indexedMap partialMakeTile


clearScreen : Float -> Float -> Renderable
clearScreen width height =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


view : Model -> Html Msg
view model =
    let
        rayList =
            getRayList model
    in
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
                        :: render3D model rayList
                        ++ renderMap model
                        ++ renderPlayer model rayList
                    )
                ]

        Nothing ->
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                []
