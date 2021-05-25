port module Main exposing (Tangents, calculateExternalTangents, main)

import Array exposing (Array)
import Array.Extra
import Browser
import Browser.Dom
import Browser.Events
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
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


port loadFontPort : String -> Cmd msg
port loadCsldCharacterPort : (String, String) -> Cmd msg
port setCsldCharacterUrlPort : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\w h -> ChangeOrientation (toFloat w) (toFloat h))
        , setCsldCharacterUrlPort SetCsldCharacterUrl
        ]


type alias Model =
    { strokes : Array Stroke
    , strokeColor : Color
    , strokeWidth : Float
    , grid : Grid
    , gridSize : Float
    , buttonHeight : Int
    , spacing : Int
    , palette : Palette
    , character : Char
    , pendingString : String
    , practiceStyle : PracticeStyle
    , orientation : Orientation
    , fontId : FontId
    , showFontSelection : Bool
    , csldCharacterUrl : Maybe String
    }


type alias FontId =
    String


type Orientation
    = Portrait
    | Landscape


type PracticeStyle
    = CopyStyle
    | ImitateStyle


type Msg
    = StartAt Point
    | ExtendAt Point
    | EndAt Point
    | UndoStroke
    | ClearStroke
    | ChangeGrid Grid
    | ChangeCharacter String
    | ChangePracticeStyle PracticeStyle
    | ChangeOrientation Float Float
    | ChangeDimensions Float Float
    | ToggleFontSelection
    | ChangeFont FontId
    | SetCsldCharacterUrl String


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
    let
        fontId =
            "edukai"

        model =
            { strokes = Array.empty
            , strokeColor = Color.black
            , strokeWidth = 20
            , grid = TianGrid
            , gridSize = 380
            , buttonHeight = 50
            , spacing = 20
            , palette =
                { darkBg = Color.rgb255 255 255 255
                , lightBg = Color.rgb255 255 255 255
                , darkFg = Color.rgb255 0 0 0
                , lightFg = Color.rgb255 90 0 0
                }
            , character = '字'
            , pendingString = ""
            , practiceStyle = CopyStyle
            , orientation = Portrait
            , fontId = fontId
            , showFontSelection = False
            , csldCharacterUrl = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform (\{ viewport } -> ChangeDimensions viewport.width viewport.height) Browser.Dom.getViewport
        , Tuple.second <| changeFont fontId model
        ]
    )


fontIds : List FontId
fontIds =
    [ "edukai", "fzkai", "i_ngaan", "eduli", "edusong", "dfsong", "fzsong", "qiji", "shuowen", "i_pen_crane", "seto", "csld_楷書", "csld_行書", "csld_草書", "csld_隸書", "csld_篆書", "csld_金文", "csld_甲骨文" ]


getFontName : FontId -> String
getFontName id =
    case id of
        "edukai" ->
            "臺灣標楷"

        "fzkai" ->
            "方正楷体"

        "i_ngaan" ->
            "I.顏體"

        "eduli" ->
            "臺灣標隸"

        "edusong" ->
            "臺灣標宋"

        "dfsong" ->
            "華康標宋"

        "fzsong" ->
            "方正新书宋"

        "qiji" ->
            "齊伋體"

        "shuowen" ->
            "說文小篆"

        "i_pen_crane" ->
            "I.鋼筆鶴體"

        "seto" ->
            "Seto手寫體"

        "csld_楷書"->
            "張炳煌楷書"
        
        "csld_隸書"->
            "張炳煌隸書"
        
        "csld_篆書"->
            "張炳煌篆書"

        "csld_行書"->
            "張炳煌行書"

        "csld_草書"->
            "張炳煌草書"

        "csld_金文"->
            "張炳煌金文"

        "csld_甲骨文"->
            "張炳煌甲文"

        _ ->
            "Invalid font id"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFont id ->
            changeFont id model
        
        ChangeCharacter string ->
            changeCharacter string model

        _ ->
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

                ChangePracticeStyle style ->
                    changePracticeStyle style model

                ChangeDimensions width height ->
                    changeDimensions width height model

                ChangeOrientation width height ->
                    changeOrientation width height model

                ToggleFontSelection ->
                    toggleFontSelection model

                SetCsldCharacterUrl url ->
                    setCsldCharacterUrl url model

                _ ->
                    model
            , Cmd.none
            )


setCsldCharacterUrl : String -> Model -> Model
setCsldCharacterUrl url model =
    { model
        | csldCharacterUrl =
            if String.isEmpty url then
                Nothing
            else
                Just url
    }


changeFont : FontId -> Model -> ( Model, Cmd Msg )
changeFont id model =
    ( { model
        | fontId =
            id
        , csldCharacterUrl =
            Nothing
      }
    , Cmd.batch
        [ if String.startsWith "csld_" id then
            Cmd.none
        else
            loadFontPort id
        , loadCsldCharacter id model.character
        ]
    )


toggleFontSelection : Model -> Model
toggleFontSelection model =
    { model
        | showFontSelection =
            not model.showFontSelection
    }


changePracticeStyle : PracticeStyle -> Model -> Model
changePracticeStyle style model =
    { model
        | practiceStyle =
            style
    }


changeOrientation : Float -> Float -> Model -> Model
changeOrientation width height model =
    { model
        | orientation =
            if width > height then
                Landscape

            else
                Portrait
    }


changeDimensions : Float -> Float -> Model -> Model
changeDimensions width height model =
    changeOrientation width height <|
        changePracticeStyle model.practiceStyle <|
            -- phone screen
            if height < model.gridSize * 3 then
                { model
                    | strokeWidth =
                        45
                    , gridSize =
                        height / 3
                    , spacing =
                        10
                }

            else
                model


changeCharacter : String -> Model -> (Model, Cmd Msg)
changeCharacter string model =
    let
        newChar =
            case String.uncons string of
                Just ( firstChar, _ ) ->
                    firstChar

                Nothing ->
                    model.character
    in
    ({ model
        | character =
            newChar
        , pendingString =
            string
    }
    , loadCsldCharacter model.fontId newChar
    )


loadCsldCharacter : String -> Char -> Cmd Msg
loadCsldCharacter fontId char =
    if String.startsWith "csld_" fontId then
        loadCsldCharacterPort ((String.dropLeft (String.length "csld_") fontId), String.fromChar char)
    else
        Cmd.none


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
            [ E.centerX, E.centerY, E.spacing model.spacing ]
        <|
            (case model.practiceStyle of
                CopyStyle ->
                    [ E.el
                        [ E.inFront <| viewStrokes model
                        , E.centerX
                        ]
                        (viewCharacter model)
                    ]

                ImitateStyle ->
                    case model.orientation of
                        Portrait ->
                            [ E.el
                                [ E.centerX ]
                                (viewCharacter model)
                            , viewWritingPad model
                            ]

                        Landscape ->
                            [ E.row
                                [ E.centerX ]
                                [ viewCharacter model
                                , viewWritingPad model
                                ]
                            ]
            )
                ++ [ viewControls model ]


viewWritingPad : Model -> E.Element Msg
viewWritingPad ({ grid, gridSize, palette } as model) =
    let
        s =
            gridSize
    in
    E.el
        ([ E.inFront <| viewStrokes model
         , E.behindContent <| viewGrid palette.lightFg 1 gridSize grid
         ]
            ++ writingPadAttributes
        )
        (E.html
            (Svg.svg [ SvgAttributes.width <| px s, SvgAttributes.height <| px s, SvgAttributes.viewBox 0 0 s s ]
                [ Svg.rect
                    [ SvgAttributes.x <| px 0
                    , SvgAttributes.y <| px 0
                    , SvgAttributes.width <| px s
                    , SvgAttributes.height <| px s
                    , SvgAttributes.fill <| Paint palette.lightBg
                    ]
                    []
                ]
            )
        )


viewControls : Model -> E.Element Msg
viewControls ({ buttonHeight, palette, gridSize, spacing } as model) =
    E.column
        [ E.spacing spacing
        , E.width <| E.px <| round gridSize
        , E.centerX
        ]
        [ E.row
            [ E.spacing spacing
            , E.centerX
            ]
            [ viewUndoButton buttonHeight palette
            , viewClearButton buttonHeight palette
            , viewSelectCopyStyleButton model
            , viewSelectImitateStyleButton model
            ]
        , viewGridSelection model
        , E.row
            [ E.spacing 10
            , E.centerX
            ]
            [ viewCharacterInput model
            , viewFontConfig model
            ]
        ]


viewFontConfig : Model -> E.Element Msg
viewFontConfig model =
    Input.button
        [ Background.color <| toElmUiColor model.palette.lightFg
        , Font.color <| toElmUiColor model.palette.darkBg
        , E.height <| E.px model.buttonHeight
        , E.width <| E.px 120
        , E.above <|
            if model.showFontSelection then
                E.column [] <|
                    List.map
                        (\id ->
                            Input.button
                                [ Background.color <| toElmUiColor <| Color.Manipulate.fadeOut 0.5 model.palette.lightFg
                                , Font.color <| toElmUiColor model.palette.darkBg
                                , E.height <| E.px <| round <| toFloat model.buttonHeight * 0.8
                                , E.width <| E.px 120
                                ]
                                { onPress = Just <| ChangeFont id
                                , label =
                                    E.text <| getFontName id
                                }
                        )
                    <|
                        List.filter ((/=) model.fontId) fontIds

            else
                E.none
        ]
        { onPress = Just ToggleFontSelection
        , label =
            E.text <| getFontName model.fontId
        }


viewCharacterInput : Model -> E.Element Msg
viewCharacterInput model =
    Input.text
        [ E.width <| E.px 120
        ]
        { onChange = ChangeCharacter
        , text = model.pendingString
        , placeholder = Just <| Input.placeholder [] <| E.text "輸入漢字"
        , label = Input.labelHidden "輸入漢字 Input Hanzi"
        }


viewUndoButton : Int -> Palette -> E.Element Msg
viewUndoButton buttonHeight palette =
    viewIconButton buttonHeight palette UndoStroke FeatherIcons.cornerUpLeft


viewClearButton : Int -> Palette -> E.Element Msg
viewClearButton buttonHeight palette =
    viewIconButton buttonHeight palette ClearStroke FeatherIcons.x


viewSelectCopyStyleButton : Model -> E.Element Msg
viewSelectCopyStyleButton { buttonHeight, practiceStyle, palette } =
    let
        newPalette =
            case practiceStyle of
                CopyStyle ->
                    palette

                ImitateStyle ->
                    { palette
                        | lightFg =
                            Color.Manipulate.fadeOut 0.5 <| palette.lightFg
                    }
    in
    viewIconButton buttonHeight newPalette (ChangePracticeStyle CopyStyle) FeatherIcons.edit


viewSelectImitateStyleButton : Model -> E.Element Msg
viewSelectImitateStyleButton { buttonHeight, practiceStyle, palette } =
    let
        newPalette =
            case practiceStyle of
                ImitateStyle ->
                    palette

                CopyStyle ->
                    { palette
                        | lightFg =
                            Color.Manipulate.fadeOut 0.5 <| palette.lightFg
                    }
    in
    viewIconButton buttonHeight newPalette (ChangePracticeStyle ImitateStyle) FeatherIcons.edit2


viewIconButton : Int -> Palette -> Msg -> FeatherIcons.Icon -> E.Element Msg
viewIconButton buttonHeight { lightFg, darkBg } msg icon =
    Input.button
        []
        { onPress = Just msg
        , label =
            E.el
                [ Font.color <| toElmUiColor darkBg
                , Background.color <| toElmUiColor lightFg
                , E.width <| E.px buttonHeight
                , E.height <| E.px buttonHeight
                ]
            <|
                E.html
                    (icon
                        |> FeatherIcons.withSize 40
                        |> FeatherIcons.withStrokeWidth 2.5
                        |> FeatherIcons.toHtml [ Html.Attributes.style "margin" "auto" ]
                    )
        }


viewGridSelection : Model -> E.Element Msg
viewGridSelection { grid, palette, spacing, buttonHeight } =
    let
        { lightFg } =
            palette

        selectedColor =
            lightFg

        selectedStrokeWidth =
            4

        unselectedColor =
            Color.Manipulate.fadeOut 0.5 <| lightFg

        unselectedStrokeWidth =
            3
    in
    E.row
        [ E.spacing spacing
        , E.centerX
        ]
    <|
        List.map
            (\currentGrid ->
                Input.button
                    []
                    { onPress = Just <| ChangeGrid currentGrid
                    , label =
                        let
                            ( color, strokeWidth ) =
                                if grid == currentGrid then
                                    ( selectedColor, selectedStrokeWidth )

                                else
                                    ( unselectedColor, unselectedStrokeWidth )
                        in
                        viewGrid color strokeWidth (toFloat buttonHeight) currentGrid
                    }
            )
            [ TianGrid, MiGrid, JingGrid, KongGrid ]


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
                                    viewPoint model.strokeColor model.strokeWidth point
                                )
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

        tangents =
            Array.Extra.zip t1 t2

        segments =
            Array.Extra.indexedMapToList
                (\index ( p1, p2 ) ->
                    let
                        x1 =
                            Vector2.getX p1

                        y1 =
                            Vector2.getY p1

                        x2 =
                            Vector2.getX p2

                        y2 =
                            Vector2.getY p2

                        { x3, y3, x4, y4 } =
                            case Array.get (index + 1) tangents of
                                Just ( p4, p3 ) ->
                                    { x3 =
                                        Vector2.getX p3
                                    , y3 =
                                        Vector2.getY p3
                                    , x4 =
                                        Vector2.getX p4
                                    , y4 =
                                        Vector2.getY p4
                                    }

                                Nothing ->
                                    { x3 = x2, y3 = y2, x4 = x1, y4 = y1 }
                    in
                    Svg.polygon
                        [ SvgAttributes.points
                            [ ( x1, y1 )
                            , ( x2, y2 )
                            , ( x3, y3 )
                            , ( x4, y4 )
                            ]
                        ]
                        []
                )
                tangents
    in
    Svg.g
        [ SvgAttributes.fill <| Paint strokeColor
        , SvgAttributes.stroke <| PaintNone
        ]
        segments


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

                    Err smallerCircle ->
                        ( smallerCircle, t1, t2 )
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
    if abs (r1 - r2) < d then
        Ok ()

    else if r1 < r2 then
        Err circle2

    else
        Err circle1


{-| Calculates the two external tangent lines
REQUIRE: | r1 - r2 | < d
-}
calculateExternalTangents : Circle -> Circle -> Tangents
calculateExternalTangents ( c1, r1 ) ( c2, r2 ) =
    let
        -- distance between the centers of two circles
        d =
            Vector2.distance c1 c2

        h =
            sqrt (d ^ 2 - (r1 - r2) ^ 2)

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
viewCharacter { practiceStyle, gridSize, grid, palette, character, fontId, csldCharacterUrl } =
    E.el
        ([ Font.size <| round gridSize
         , Font.family
            [ Font.typeface fontId
            ]
         , E.centerX
         , E.width <| E.px <| round gridSize
         , E.behindContent <| viewGrid palette.lightFg 1 gridSize grid
         , E.htmlAttribute <| Html.Attributes.style "user-select" "none"
         , case practiceStyle of
            CopyStyle ->
                Font.color <| toElmUiColor <| Color.Manipulate.fadeOut 0.5 palette.darkFg

            ImitateStyle ->
                Font.color <| toElmUiColor palette.darkFg
         ]
            ++ (case practiceStyle of
                    CopyStyle ->
                        writingPadAttributes

                    ImitateStyle ->
                        []
               )
        )
    <|
        case csldCharacterUrl of
            Just url ->
                E.image [ E.centerX, E.width E.fill ]
                    { src =
                        url
                    , description =
                        String.cons character ("（" ++ getFontName fontId ++ "）")
                    }
            Nothing ->
                E.text <|
                    String.fromChar character


writingPadAttributes : List (E.Attribute Msg)
writingPadAttributes =
    [ E.htmlAttribute <| Html.Attributes.style "user-select" "none"
    , E.htmlAttribute <| Pointer.onDown (decodePoint >> StartAt)
    , E.htmlAttribute <| Pointer.onMove (decodePoint >> ExtendAt)
    , E.htmlAttribute <| Pointer.onUp (decodePoint >> EndAt)

    -- no touch-action (prevent scroll etc.)
    , E.htmlAttribute <| Html.Attributes.style "touch-action" "none"
    ]


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
