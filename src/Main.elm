module Main exposing (Tangents, calculateExternalTangents, main)

import Array exposing (Array)
import Array.Extra
import Browser
import Color exposing (Color)
import Color.Manipulate
import Element as E
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Math.Vector2 as Vector2 exposing (Vec2)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { strokes : Array Stroke
    , strokeColor : Color
    , strokeWidth : Float
    , grid : Grid
    , gridSize : Float
    , textSize : Float
    , palette : Palette
    , character : Char
    , pendingString : String
    , showCharacter : Bool
    }


type Msg
    = StartAt Point
    | ExtendAt Point
    | EndAt Point
    | UndoStroke
    | ClearStroke
    | ChangeGrid Grid
    | ChangeCharacter String


type alias Palette =
    { darkFg : Color
    , lightFg : Color
    , darkBg : Color
    , lightBg : Color
    }


type alias Stroke =
    Array Point


type alias Point =
    { x : Float
    , y : Float
    , force : Float
    }


type Grid
    = TianGrid
    | MiGrid
    | JingGrid
    | KongGrid


init : () -> ( Model, Cmd Msg )
init _ =
    ( { strokes = Array.empty
      , strokeColor = Color.black
      , strokeWidth = 20
      , grid = TianGrid
      , gridSize = 400
      , textSize = 50
      , palette =
            { darkBg = Color.rgb255 255 255 255
            , lightBg = Color.rgb255 255 255 255
            , darkFg = Color.rgb255 0 0 0
            , lightFg = Color.rgb255 90 0 0
            }
      , character = '龍'
      , pendingString = ""
      , showCharacter = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        StartAt point ->
            startStroke point model

        ExtendAt point ->
            extendStroke point model

        EndAt point ->
            endStroke point model

        UndoStroke ->
            undoStroke model

        ClearStroke ->
            clearStroke model

        ChangeGrid grid ->
            changeGrid grid model

        ChangeCharacter string ->
            changeCharacter string model
    , Cmd.none
    )


changeCharacter : String -> Model -> Model
changeCharacter string model =
    { model
        | character =
            case String.uncons string of
                Just ( firstChar, _ ) ->
                    firstChar

                Nothing ->
                    model.character
        , pendingString =
            string
    }


changeGrid : Grid -> Model -> Model
changeGrid grid model =
    { model
        | grid =
            grid
    }


clearStroke : Model -> Model
clearStroke model =
    { model
        | strokes =
            Array.empty
    }


undoStroke : Model -> Model
undoStroke model =
    { model
        | strokes =
            Array.Extra.pop model.strokes
    }


startStroke : Point -> Model -> Model
startStroke point model =
    let
        newStroke =
            Array.fromList [ point ]
    in
    { model
        | strokes =
            Array.push newStroke model.strokes
    }


extendStroke : Point -> Model -> Model
extendStroke point model =
    if point.force == 0 then
        model

    else
        { model
            | strokes =
                Array.Extra.update
                    (Array.length model.strokes - 1)
                    (\stroke ->
                        Array.push point stroke
                    )
                    model.strokes
        }


endStroke : Point -> Model -> Model
endStroke point model =
    extendStroke point model


view : Model -> Html Msg
view model =
    E.layout [] <|
        E.column
            [ E.centerX, E.centerY, E.spacing 30 ]
            [ E.el
                [ E.inFront <| viewStrokes model ]
                (viewCharacter model)
            , viewControls model
            ]


viewControls : Model -> E.Element Msg
viewControls model =
    E.column
        [ E.spacing 20 ]
        [ E.row
            [ E.spacing 20 ]
            [ viewUndoButton model.palette
            , viewClearButton model.palette
            , viewGridSelection model.palette
            ]
        , viewCharacterInput model
        ]


viewCharacterInput : Model -> E.Element Msg
viewCharacterInput model =
    Input.text []
        { onChange = ChangeCharacter
        , text = model.pendingString
        , placeholder = Just <| Input.placeholder [] <| E.text "輸入漢字"
        , label = Input.labelHidden "輸入漢字 Input Hanzi"
        }


viewUndoButton : Palette -> E.Element Msg
viewUndoButton { lightFg, darkBg } =
    Input.button
        []
        { onPress = Just UndoStroke
        , label =
            E.el
                [ Font.color <| toElmUiColor darkBg
                , Background.color <| toElmUiColor lightFg
                ]
            <|
                E.html
                    (FeatherIcons.cornerUpLeft
                        |> FeatherIcons.withSize 50
                        |> FeatherIcons.withStrokeWidth 3
                        |> FeatherIcons.toHtml []
                    )
        }


viewClearButton : Palette -> E.Element Msg
viewClearButton { lightFg, darkBg } =
    Input.button
        []
        { onPress = Just ClearStroke
        , label =
            E.el
                [ Font.color <| toElmUiColor darkBg
                , Background.color <| toElmUiColor lightFg
                ]
            <|
                E.html
                    (FeatherIcons.x
                        |> FeatherIcons.withSize 50
                        |> FeatherIcons.withStrokeWidth 3
                        |> FeatherIcons.toHtml []
                    )
        }


viewGridSelection : Palette -> E.Element Msg
viewGridSelection { lightFg, darkBg } =
    E.row
        [ E.spacing 20 ]
        [ Input.button
            []
            { onPress = Just <| ChangeGrid TianGrid
            , label =
                viewGrid lightFg 2 50 TianGrid
            }
        , Input.button
            []
            { onPress = Just <| ChangeGrid MiGrid
            , label =
                viewGrid lightFg 2 50 MiGrid
            }
        , Input.button
            []
            { onPress = Just <| ChangeGrid JingGrid
            , label =
                viewGrid lightFg 2 50 JingGrid
            }
        , Input.button
            []
            { onPress = Just <| ChangeGrid KongGrid
            , label =
                viewGrid lightFg 2 50 KongGrid
            }
        ]


viewStrokes : Model -> E.Element Msg
viewStrokes model =
    E.html <|
        Svg.svg
            [ SvgAttributes.viewBox 0 0 model.gridSize model.gridSize
            , SvgAttributes.style "pointer-events: none"
            ]
        <|
            Array.Extra.mapToList
                (\stroke ->
                    Svg.g [] <|
                        viewTangents model.strokeColor model.strokeWidth stroke
                            :: Array.Extra.mapToList
                                (\point ->
                                    -- case point of
                                    --     Nothing ->
                                    --         Svg.g [] []

                                    --     -- empty svg element
                                    --     Just p ->
                                            viewPoint model.strokeColor model.strokeWidth point
                                )
                                -- [ Array.get 0 stroke, Array.get (Array.length stroke - 1) stroke ]
                                stroke
                )
                model.strokes


viewTangents : Color -> Float -> Stroke -> Svg Msg
viewTangents strokeColor strokeWidth stroke =
    let
        ( t1, t2 ) =
            calculateTangents <|
                Array.map
                    (\{ x, y, force } ->
                        ( Vector2.vec2 x y, force * strokeWidth )
                    )
                    stroke

        tangentPoints =
            Array.toList <|
                Array.map
                    (\vec2 ->
                        let
                            { x, y } =
                                Vector2.toRecord vec2
                        in
                        ( x, y )
                    )
                <|
                    Array.append
                        t1
                        (Array.fromList <| List.reverse <| Array.toList t2)
    in
    Svg.polygon
        [ SvgAttributes.fill <| Paint strokeColor
        , SvgAttributes.stroke <| PaintNone
        , SvgAttributes.points tangentPoints
        ]
        []


type alias Circle =
    ( Vec2, Float )


{-| Calculate the external tangents of an array of circles

    REQUIRE: Array.length circles >= 2

-}
calculateTangents : Array Circle -> ( Array Vec2, Array Vec2 )
calculateTangents circles =
    let
        -- must have at least 2 circles so withDefault should never happen
        firstCircle =
            Maybe.withDefault ( Vector2.vec2 0 0, 0 ) <| Array.get 0 circles
    in
    tuple3Tail <|
        Array.foldl
            (\circle ( prevCircle, t1, t2 ) ->
                -- prevCircle and circle share the same center
                case hasExternalTangents prevCircle circle of
                    Ok _ ->
                        let
                            ( ( s1p1, s1p2 ), ( s2p1, s2p2 ) ) =
                                calculateExternalTangents prevCircle circle

                            s1 =
                                Array.fromList [ s1p1, s1p2 ]

                            s2 =
                                Array.fromList [ s2p1, s2p2 ]
                        in
                        if Array.isEmpty t1 then
                            ( circle, s1, s2 )

                        else
                            ( circle, Array.append t1 s1, Array.append t2 s2 )
                    
                    Err largerCircle ->
                        ( circle, t1, t2 )
            )
            ( firstCircle
            , Array.empty
            , Array.empty
            )
            (Array.Extra.sliceFrom 1 circles)


tuple3Tail : ( a, b, c ) -> ( b, c )
tuple3Tail ( a, b, c ) =
    ( b, c )


type alias Tangents =
    ( ( Vec2, Vec2 ), ( Vec2, Vec2 ) )


hasExternalTangents : Circle -> Circle -> Result Circle ()
hasExternalTangents (( c1, r1 ) as circle1) (( c2, r2 ) as circle2) =
    let
        d =
            Vector2.distance c1 c2
    in
    if abs (r1 - r2) < d && d < r1 + r2 then
        Ok ()

    else if r1 < r2 then
        Err circle1

    else
        Err circle2


{-| Calculates the two external tangent lines
REQUIRE: | r1 - r2 | < d < r1 + r2
-}
calculateExternalTangents : Circle -> Circle -> Tangents
calculateExternalTangents ( c1, r1 ) ( c2, r2 ) =
    let
        -- distance between the centers of two circles
        d =
            Vector2.distance c1 c2

        h =
            sqrt (d ^ 2 - (r1 - r2) ^ 2)

        -- _ =
            -- Debug.log "h" h

        y =
            sqrt (h ^ 2 + r2 ^ 2)

        x1 =
            Vector2.getX c1

        y1 =
            Vector2.getY c1

        x2 =
            Vector2.getX c2

        y2 =
            Vector2.getY c2

        rawTheta =
            acos ((r1 ^ 2 + d ^ 2 - y ^ 2) / (2 * r1 * d))

        xTilt =
            atan2 (y2 - y1) (x2 - x1)

        -- tangent line 1 with two points
        theta =
            rawTheta + xTilt

        t1 =
            Vector2.vec2 (x1 + r1 * cos theta) (y1 + r1 * sin theta)

        t2 =
            Vector2.vec2 (x2 + r2 * cos theta) (y2 + r2 * sin theta)

        -- tangent line 2 with two points
        t3 =
            vector2Rotate c1 (2 * rawTheta) t1

        t4 =
            vector2Rotate c2 (2 * rawTheta) t2
    in
    if (x2 < x1 || y1 > y2) && not (x2 > x1 && y1 > y2) then
        ( ( t3, t4 ), ( t1, t2 ) )

    else
        ( ( t1, t2 ), ( t3, t4 ) )


{-| Rotate a `point` around a `pivot` clockwise `angle` radians
-}
vector2Rotate : Vec2 -> Float -> Vec2 -> Vec2
vector2Rotate pivot angle point =
    let
        s =
            sin angle

        c =
            cos angle
    in
    Vector2.add pivot <|
        (\vec ->
            let
                { x, y } =
                    Vector2.toRecord vec

                newX =
                    x * c + y * s

                newY =
                    -x * s + y * c
            in
            Vector2.vec2 newX newY
        )
        <|
            Vector2.sub point pivot


viewPoint : Color -> Float -> Point -> Svg Msg
viewPoint color width point =
    Svg.circle
        [ SvgAttributes.cx (px point.x)
        , SvgAttributes.cy (px point.y)
        , SvgAttributes.r (px <| width * point.force)
        , SvgAttributes.fill <| Paint Color.black
        ]
        []


viewCharacter : Model -> E.Element Msg
viewCharacter { gridSize, grid, palette, character } =
    E.el
        [ Font.size 400
        , Font.family
            [ Font.typeface "Edukai"
            ]
        , Font.color <| toElmUiColor <| Color.Manipulate.fadeOut 0.5 palette.darkFg
        , E.centerX
        , E.htmlAttribute <| Html.Attributes.style "user-select" "none"
        , E.htmlAttribute <| Pointer.onDown (decodePoint >> StartAt)
        , E.htmlAttribute <| Pointer.onMove (decodePoint >> ExtendAt)
        , E.htmlAttribute <| Pointer.onUp (decodePoint >> EndAt)

        -- no touch-action (prevent scroll etc.)
        , E.htmlAttribute <| Html.Attributes.style "touch-action" "none"
        , E.behindContent <| viewGrid palette.lightFg 1 gridSize grid
        ]
    <|
        E.text <|
            String.fromChar character


viewGrid : Color -> Float -> Float -> Grid -> E.Element Msg
viewGrid strokeColor strokeWidth size grid =
    let
        -- Delta value to prevent trimming of border stroke width
        d =
            strokeWidth / 2
    in
    E.html <|
        Svg.svg
            [ SvgAttributes.viewBox 0 0 size size
            , SvgAttributes.width (px size)
            , SvgAttributes.height (px size)
            , SvgAttributes.stroke <| Paint <| strokeColor
            , SvgAttributes.strokeWidth <| px strokeWidth
            , SvgAttributes.style "pointer-events: none"
            ]
            [ Svg.g [] <|
                List.append
                    (case grid of
                        TianGrid ->
                            [ Svg.line
                                [ SvgAttributes.x1 (px <| 0)
                                , SvgAttributes.y1 (px <| size / 2)
                                , SvgAttributes.x2 (px <| size)
                                , SvgAttributes.y2 (px <| size / 2)
                                ]
                                []
                            , Svg.line
                                [ SvgAttributes.x1 (px <| size / 2)
                                , SvgAttributes.y1 (px <| 0)
                                , SvgAttributes.x2 (px <| size / 2)
                                , SvgAttributes.y2 (px <| size)
                                ]
                                []
                            ]

                        JingGrid ->
                            -- two horizontals
                            [ Svg.line
                                [ SvgAttributes.x1 (px <| 0)
                                , SvgAttributes.y1 (px <| size / 3)
                                , SvgAttributes.x2 (px <| size)
                                , SvgAttributes.y2 (px <| size / 3)
                                ]
                                []
                            , Svg.line
                                [ SvgAttributes.x1 (px <| 0)
                                , SvgAttributes.y1 (px <| 2 * size / 3)
                                , SvgAttributes.x2 (px <| size)
                                , SvgAttributes.y2 (px <| 2 * size / 3)
                                ]
                                []

                            -- two verticals
                            , Svg.line
                                [ SvgAttributes.x1 (px <| size / 3)
                                , SvgAttributes.y1 (px <| 0)
                                , SvgAttributes.x2 (px <| size / 3)
                                , SvgAttributes.y2 (px <| size)
                                ]
                                []
                            , Svg.line
                                [ SvgAttributes.x1 (px <| 2 * size / 3)
                                , SvgAttributes.y1 (px <| 0)
                                , SvgAttributes.x2 (px <| 2 * size / 3)
                                , SvgAttributes.y2 (px <| size)
                                ]
                                []
                            ]

                        MiGrid ->
                            [ Svg.line
                                [ SvgAttributes.x1 (px <| 0)
                                , SvgAttributes.y1 (px <| size / 2)
                                , SvgAttributes.x2 (px <| size)
                                , SvgAttributes.y2 (px <| size / 2)
                                ]
                                []
                            , Svg.line
                                [ SvgAttributes.x1 (px <| size / 2)
                                , SvgAttributes.y1 (px <| 0)
                                , SvgAttributes.x2 (px <| size / 2)
                                , SvgAttributes.y2 (px <| size)
                                ]
                                []
                            , Svg.line
                                [ SvgAttributes.x1 (px <| 0)
                                , SvgAttributes.y1 (px <| 0)
                                , SvgAttributes.x2 (px <| size)
                                , SvgAttributes.y2 (px <| size)
                                ]
                                []
                            , Svg.line
                                [ SvgAttributes.x1 (px <| size)
                                , SvgAttributes.y1 (px <| 0)
                                , SvgAttributes.x2 (px <| 0)
                                , SvgAttributes.y2 (px <| size)
                                ]
                                []
                            ]

                        KongGrid ->
                            []
                    )
                    [ Svg.line
                        [ SvgAttributes.x1 (px <| 0)
                        , SvgAttributes.y1 (px <| 0 + d)
                        , SvgAttributes.x2 (px <| size)
                        , SvgAttributes.y2 (px <| 0 + d)
                        ]
                        []
                    , Svg.line
                        [ SvgAttributes.x1 (px <| 0)
                        , SvgAttributes.y1 (px <| size - d)
                        , SvgAttributes.x2 (px <| size)
                        , SvgAttributes.y2 (px <| size - d)
                        ]
                        []

                    -- two verticals
                    , Svg.line
                        [ SvgAttributes.x1 (px <| 0 + d)
                        , SvgAttributes.y1 (px <| 0)
                        , SvgAttributes.x2 (px <| 0 + d)
                        , SvgAttributes.y2 (px <| size)
                        ]
                        []
                    , Svg.line
                        [ SvgAttributes.x1 (px <| size - d)
                        , SvgAttributes.y1 (px <| 0)
                        , SvgAttributes.x2 (px <| size - d)
                        , SvgAttributes.y2 (px <| size)
                        ]
                        []
                    ]
            ]


decodePoint : Pointer.Event -> Point
decodePoint event =
    let
        ( x, y ) =
            event.pointer.offsetPos

        force =
            event.contactDetails.pressure
    in
    { x = x
    , y = y
    , force = force
    }


toElmUiColor : Color -> E.Color
toElmUiColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha
