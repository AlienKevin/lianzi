module Main exposing (main)

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
    , strokeWidth : Int
    , grid : Maybe Grid
    , size : Float
    , palette : Palette
    }


type Msg
    = StartAt Point
    | ExtendAt Point
    | EndAt Point
    | UndoStroke
    | ClearStroke


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
    | JingGrid
    | MiGrid


init : () -> ( Model, Cmd Msg )
init _ =
    ( { strokes = Array.empty
      , strokeColor = Color.black
      , strokeWidth = 20
      , grid = Just JingGrid
      , size = 400
      , palette =
            { darkBg = Color.rgb255 255 255 255
            , lightBg = Color.rgb255 139 0 0
            , darkFg = Color.rgb255 0 0 0
            , lightFg = Color.rgb255 0 0 0
            }
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
    , Cmd.none
    )


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
    E.row
        [ E.spacing 20 ]
        [ viewUndoButton model.palette
        , viewClearButton model.palette
        ]


viewUndoButton : Palette -> E.Element Msg
viewUndoButton { lightBg, darkBg } =
    Input.button
        []
        { onPress = Just UndoStroke
        , label =
            E.el
                [ Font.color <| toElmUiColor darkBg
                , Background.color <| toElmUiColor lightBg
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
viewClearButton { lightBg, darkBg } =
    Input.button
        []
        { onPress = Just ClearStroke
        , label =
            E.el
                [ Font.color <| toElmUiColor darkBg
                , Background.color <| toElmUiColor lightBg
                ]
            <|
                E.html
                    (FeatherIcons.x
                        |> FeatherIcons.withSize 50
                        |> FeatherIcons.withStrokeWidth 3
                        |> FeatherIcons.toHtml []
                    )
        }


viewStrokes : Model -> E.Element Msg
viewStrokes model =
    E.html <|
        Svg.svg
            [ SvgAttributes.viewBox 0 0 model.size model.size
            , SvgAttributes.style "pointer-events: none"
            ]
        <|
            Array.Extra.mapToList
                (\stroke ->
                    Svg.g [] <|
                        Array.Extra.mapToList
                            (viewPoint model.strokeColor model.strokeWidth)
                            stroke
                )
                model.strokes


viewPoint : Color -> Int -> Point -> Svg Msg
viewPoint color width point =
    Svg.circle
        [ SvgAttributes.cx (px point.x)
        , SvgAttributes.cy (px point.y)
        , SvgAttributes.r (px <| toFloat width * point.force)
        , SvgAttributes.fill <| Paint Color.black
        ]
        []


viewCharacter : Model -> E.Element Msg
viewCharacter { size, grid, palette } =
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
        , E.behindContent <|
            case grid of
                Just g ->
                    viewGrid palette size g

                Nothing ->
                    E.none
        ]
    <|
        E.text "龍"


viewGrid : Palette -> Float -> Grid -> E.Element Msg
viewGrid palette size grid =
    E.html <|
        Svg.svg
            [ SvgAttributes.viewBox 0 0 size size
            , SvgAttributes.style "pointer-events: none"
            , SvgAttributes.stroke <| Paint <| palette.lightBg
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
                    )
                    [ Svg.line
                        [ SvgAttributes.x1 (px <| 0)
                        , SvgAttributes.y1 (px <| 0)
                        , SvgAttributes.x2 (px <| size)
                        , SvgAttributes.y2 (px <| 0)
                        ]
                        []
                    , Svg.line
                        [ SvgAttributes.x1 (px <| 0)
                        , SvgAttributes.y1 (px <| size)
                        , SvgAttributes.x2 (px <| size)
                        , SvgAttributes.y2 (px <| size)
                        ]
                        []

                    -- two verticals
                    , Svg.line
                        [ SvgAttributes.x1 (px <| 0)
                        , SvgAttributes.y1 (px <| 0)
                        , SvgAttributes.x2 (px <| 0)
                        , SvgAttributes.y2 (px <| size)
                        ]
                        []
                    , Svg.line
                        [ SvgAttributes.x1 (px <| size)
                        , SvgAttributes.y1 (px <| 0)
                        , SvgAttributes.x2 (px <| size)
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
