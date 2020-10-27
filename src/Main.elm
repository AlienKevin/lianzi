module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Color exposing (Color)
import Element as E
import Element.Font as Font
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
    }


type Msg
    = StartAt Point
    | ExtendAt Point
    | EndAt Point


type alias Stroke =
    Array Point


type alias Point =
    { x : Float
    , y : Float
    , force : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { strokes = Array.empty
      , strokeColor = Color.black
      , strokeWidth = 20
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
    , Cmd.none
    )


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
                viewCharacter
            ]


viewStrokes : Model -> E.Element Msg
viewStrokes model =
    E.html <|
        Svg.svg [ SvgAttributes.viewBox 0 0 400 400 
            , SvgAttributes.style "pointer-events: none"
        ] <|
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


viewCharacter : E.Element Msg
viewCharacter =
    E.el
        [ Font.size 400
        , Font.family
            [ Font.typeface "Edukai"
            ]
        , Font.color <| toElmUiColor <| Color.rgba 0 0 0 0.5
        , E.centerX
        , E.htmlAttribute <| Pointer.onDown (decodePoint >> StartAt)
        , E.htmlAttribute <| Pointer.onMove (decodePoint >> ExtendAt)
        , E.htmlAttribute <| Pointer.onUp (decodePoint >> EndAt)

        -- no touch-action (prevent scroll etc.)
        , E.htmlAttribute <| Html.Attributes.style "touch-action" "none"
        ]
    <|
        E.text "龍"


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
        {red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha