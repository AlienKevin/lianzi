module Tests exposing (..)

import Expect exposing (Expectation)
import Main exposing (Tangents, calculateExternalTangents)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Test exposing (Test, describe, test)


tangentsEqual : Tangents -> Tangents -> Expectation
tangentsEqual (( t1, t2 ) as tp1) (( t3, t4 ) as tp2) =
    let
        ( t11, t12 ) =
            t1

        ( t21, t22 ) =
            t2

        ( t31, t32 ) =
            t3

        ( t41, t42 ) =
            t4
    in
    if
        vector2Equal t11 t31
            && vector2Equal t12 t32
            && vector2Equal t21 t41
            && vector2Equal t22 t42
    then
        Expect.pass

    else
        Expect.fail <| tangentToString tp2 ++ "\n does not match\n" ++ tangentToString tp1


tangentToString : Tangents -> String
tangentToString ( t1, t2 ) =
    let
        ( t11, t12 ) =
            t1

        ( t21, t22 ) =
            t2
    in
    vec2ToString t11
        ++ " -> "
        ++ vec2ToString t12
        ++ "\n"
        ++ vec2ToString t21
        ++ " -> "
        ++ vec2ToString t22


vec2ToString : Vec2 -> String
vec2ToString v =
    let
        { x, y } =
            Vector2.toRecord v
    in
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


vector2Equal : Vec2 -> Vec2 -> Bool
vector2Equal v1 v2 =
    floatEqual (Vector2.getX v1) (Vector2.getX v2)
        && floatEqual (Vector2.getY v1) (Vector2.getY v2)


floatEqual : Float -> Float -> Bool
floatEqual f1 f2 =
    abs (f1 - f2) < 0.01


suite : Test
suite =
    describe "The Main module"
        [ describe "calculateExternalTangents"
            [ describe "Equal radii circles"
                [ test "Horizontal" <|
                    \_ ->
                        let
                            r =
                                10

                            c1 =
                                ( vec2 0 0, r )

                            c2 =
                                ( vec2 40 0, r )

                            t1 =
                                ( vec2 0 10, vec2 40 10 )

                            t2 =
                                ( vec2 0 -10, vec2 40 -10 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Vertical" <|
                    \_ ->
                        let
                            r =
                                10

                            c1 =
                                ( vec2 0 0, r )

                            c2 =
                                ( vec2 0 40, r )

                            t1 =
                                ( vec2 -10 0, vec2 -10 40 )

                            t2 =
                                ( vec2 10 0, vec2 10 40 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal" <|
                    \_ ->
                        let
                            r =
                                10

                            c1 =
                                ( vec2 0 0, r )

                            c2 =
                                ( vec2 40 40, r )

                            d =
                                sqrt 2 / 2 * 10

                            t1 =
                                ( vec2 -d d, vec2 (40 - d) (40 + d) )

                            t2 =
                                ( vec2 d -d, vec2 (40 + d) (40 - d) )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                ]
            , describe "Different radii circles"
                [ test "Horizontal (small circle c1 at left)" <|
                    \_ ->
                        let
                            r1 =
                                10

                            r2 =
                                20

                            c1 =
                                ( vec2 0 0, r1 )

                            c2 =
                                ( vec2 40 0, r2 )

                            t1 =
                                ( vec2 -2.5 9.68, vec2 35 19.36 )

                            t2 =
                                ( vec2 -2.5 -9.68, vec2 35 -19.36 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Horizontal (small circle c2 at left)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                10

                            c1 =
                                ( vec2 40 0, r1 )

                            c2 =
                                ( vec2 0 0, r2 )

                            t1 =
                                ( vec2 35 19.36, vec2 -2.5 9.68 )

                            t2 =
                                ( vec2 35 -19.36, vec2 -2.5 -9.68 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Horizontal (small circle c2 at right)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                10

                            c1 =
                                ( vec2 0 0, r1 )

                            c2 =
                                ( vec2 40 0, r2 )

                            t1 =
                                ( vec2 5 19.36, vec2 42.5 9.68 )

                            t2 =
                                ( vec2 5 -19.36, vec2 42.5 -9.68 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Horizontal (small circle c1 at right)" <|
                    \_ ->
                        let
                            r1 =
                                10

                            r2 =
                                20

                            c1 =
                                ( vec2 40 0, r1 )

                            c2 =
                                ( vec2 0 0, r2 )

                            t1 =
                                ( vec2 42.5 9.68, vec2 5 19.36 )

                            t2 =
                                ( vec2 42.5 -9.68, vec2 5 -19.36 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Vertical (small circle c1 at bottom)" <|
                    \_ ->
                        let
                            r1 =
                                10

                            r2 =
                                20

                            c1 =
                                ( vec2 0 0, r1 )

                            c2 =
                                ( vec2 0 40, r2 )

                            t1 =
                                ( vec2 -9.68 -2.5, vec2 -19.36 35 )

                            t2 =
                                ( vec2 9.68 -2.5, vec2 19.36 35 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Vertical (small circle c2 at bottom)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                10

                            c1 =
                                ( vec2 0 40, r1 )

                            c2 =
                                ( vec2 0 0, r2 )

                            t1 =
                                ( vec2 -19.36 35, vec2 -9.68 -2.5 )

                            t2 =
                                ( vec2 19.36 35, vec2 9.68 -2.5 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Vertical (small circle c1 at top)" <|
                    \_ ->
                        let
                            r1 =
                                10

                            r2 =
                                20

                            c1 =
                                ( vec2 0 40, r1 )

                            c2 =
                                ( vec2 0 0, r2 )

                            t1 =
                                ( vec2 -9.68 42.5, vec2 -19.36 5 )

                            t2 =
                                ( vec2 9.68 42.5, vec2 19.36 5 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Vertical (small circle c2 at top)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                10

                            c1 =
                                ( vec2 0 0, r1 )

                            c2 =
                                ( vec2 0 40, r2 )

                            t1 =
                                ( vec2 -19.36 5, vec2 -9.68 42.5 )

                            t2 =
                                ( vec2 19.36 5, vec2 9.68 42.5 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 45deg (small circle c1 at bottom left)" <|
                    \_ ->
                        let
                            r1 =
                                10

                            r2 =
                                20

                            c1 =
                                ( vec2 0 0, r1 )

                            c2 =
                                ( vec2 40 40, r2 )

                            t1 =
                                ( vec2 -8.21 5.71, vec2 23.58 51.42 )

                            t2 =
                                ( vec2 5.71 -8.21, vec2 51.42 23.58 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 45deg (small circle c2 at bottom left)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                10

                            c1 =
                                ( vec2 40 40, r1 )

                            c2 =
                                ( vec2 0 0, r2 )

                            t1 =
                                ( vec2 23.58 51.42, vec2 -8.21 5.71 )

                            t2 =
                                ( vec2 51.42 23.58, vec2 5.71 -8.21 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 45deg (small circle c1 at top right)" <|
                    \_ ->
                        let
                            r1 =
                                10

                            r2 =
                                20

                            c1 =
                                ( vec2 40 40, r1 )

                            c2 =
                                ( vec2 0 0, r2 )

                            t1 =
                                ( vec2 34.29 48.21, vec2 -11.42 16.42 )

                            t2 =
                                ( vec2 48.21 34.29, vec2 16.42 -11.42 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 45deg (small circle c2 at top right)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                10

                            c1 =
                                ( vec2 0 0, r1 )

                            c2 =
                                ( vec2 40 40, r2 )

                            t1 =
                                ( vec2 -11.42 16.42, vec2 34.29 48.21 )

                            t2 =
                                ( vec2 16.42 -11.42, vec2 48.21 34.29 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 169deg (small circle c1 at top left)" <|
                    \_ ->
                        let
                            r1 =
                                5

                            r2 =
                                20

                            c1 =
                                ( vec2 -20 -30, r1 )

                            c2 =
                                ( vec2 30 -40, r2 )

                            t1 =
                                ( vec2 -20.51 -25.03, vec2 27.98 -20.1 )

                            t2 =
                                ( vec2 -22.38 -34.4, vec2 20.48 -57.59 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 169deg (small circle c2 at top left)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                5

                            c1 =
                                ( vec2 30 -40, r1 )

                            c2 =
                                ( vec2 -20 -30, r2 )

                            t1 =
                                ( vec2 27.98 -20.1, vec2 -20.51 -25.03 )

                            t2 =
                                ( vec2 20.48 -57.59, vec2 -22.38 -34.4 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 169deg (small circle c1 at bottom right)" <|
                    \_ ->
                        let
                            r1 =
                                5

                            r2 =
                                20

                            c1 =
                                ( vec2 30 -40, r1 )

                            c2 =
                                ( vec2 -20 -30, r2 )

                            t1 =
                                ( vec2 32.38 -35.6, vec2 -10.48 -12.41 )

                            t2 =
                                ( vec2 30.51 -44.97, vec2 -17.98 -49.9 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                , test "Diaganal 169deg (small circle c2 at bottom right)" <|
                    \_ ->
                        let
                            r1 =
                                20

                            r2 =
                                5

                            c1 =
                                ( vec2 -20 -30, r1 )

                            c2 =
                                ( vec2 30 -40, r2 )

                            t1 =
                                ( vec2 -10.48 -12.41, vec2 32.38 -35.6 )

                            t2 =
                                ( vec2 -17.98 -49.9, vec2 30.51 -44.97 )
                        in
                        calculateExternalTangents c1 c2
                            |> tangentsEqual ( t1, t2 )
                ]
            ]
        ]
