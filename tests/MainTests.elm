module MainTests exposing (suite)

import Array as A
import Expect exposing (Expectation)
import Main exposing (Board, Stone, check, set)
import Test exposing (..)


board : Board
board =
    A.repeat 8 (A.repeat 8 Nothing)
        |> set Main.White ( 3, 3 )
        |> set Main.White ( 4, 4 )
        |> set Main.Black ( 4, 3 )
        |> set Main.Black ( 3, 4 )
        |> set Main.White ( 2, 3 )


suite : Test
suite =
    describe "methods"
        [ describe "check"
            [ test "can turn" <|
                \_ ->
                    Main.check board Main.Black ( 5, 4 )
                        |> Expect.equal True
            , test "cannot turn" <|
                \_ ->
                    Main.check board Main.Black ( 5, 3 )
                        |> Expect.equal False
            , test "cannot turn, and nothing is around" <|
                \_ ->
                    Main.check board Main.Black ( 1, 1 )
                        |> Expect.equal False
            , test "can turn, diagnal" <|
                \_ ->
                    Main.check board Main.Black ( 1, 2 )
                        |> Expect.equal True
            , test "can not turn diagnal" <|
                \_ ->
                    Main.check board Main.White ( 1, 2 )
                        |> Expect.equal False
            , test "can not turn mulitple" <|
                \_ ->
                    Main.check board Main.White ( 1, 3 )
                        |> Expect.equal False
            , test "can turn mulitple" <|
                \_ ->
                    Main.check board Main.Black ( 1, 3 )
                        |> Expect.equal True
            , test "cannot turn when same stone in a row" <|
                \_ ->
                    Main.check board Main.Black ( 5, 2 )
                        |> Expect.equal False
            , test "cannot turn when there is no matching stone at the end" <|
                \_ ->
                    Main.check board Main.White ( 5, 2 )
                        |> Expect.equal False
            ]
        ]
