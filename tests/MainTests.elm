module MainTests exposing (suite)

import Array as A
import Expect exposing (Expectation)
import Main exposing (Board, Stone, check, set)
import Test exposing (..)


board : Board
board =
    Debug.log "testboard"
        (A.repeat 8 (A.repeat 8 Nothing)
            |> set Main.White ( 3, 3 )
            |> set Main.White ( 4, 4 )
            |> set Main.Black ( 4, 3 )
            |> set Main.Black ( 3, 3 )
        )


suite : Test
suite =
    describe "methods"
        [ describe "check"
            [ test "can turn" <|
                \_ ->
                    Main.check board Main.Black ( 5, 4 )
                        |> Expect.equal True

            -- , test "cannot turn" <|
            --     \_ ->
            --         Main.check board Main.Black ( 5, 3 )
            --             |> Expect.equal False
            -- , test "cannot turn, and nothing is around" <|
            --     \_ ->
            --         Main.check board Main.Black ( 1, 1 )
            --             |> Expect.equal False
            ]
        ]
