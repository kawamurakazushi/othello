module Main exposing (Board, Model, Msg(..), Stone(..), board, check, init, main, set, stone, update, view)

import Array as A exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List as L



-- ---------------------------
-- MODEL
-- ---------------------------


type Stone
    = Black
    | White


opposite : Stone -> Stone
opposite s =
    case s of
        Black ->
            White

        White ->
            Black


type alias Board =
    Array (Array (Maybe Stone))


type alias Model =
    { board : Board
    , player : Stone
    }


type alias Point =
    ( Int, Int )


getPoint : Point -> Board -> Maybe Stone
getPoint ( col, row ) b =
    A.get row b
        |> Maybe.withDefault ([] |> A.fromList)
        |> A.get col
        |> Maybe.withDefault Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board =
            A.repeat 8 (A.repeat 8 Nothing)
                |> set White ( 3, 3 )
                |> set White ( 4, 4 )
                |> set Black ( 4, 3 )
                |> set Black ( 3, 4 )
      , player = Black
      }
    , Cmd.none
    )



-- ---------------------------
-- MSG
-- ---------------------------


type Msg
    = Set Stone Point



-- ---------------------------
-- UPDATE
-- ---------------------------


set : Stone -> Point -> Board -> Board
set s ( x, y ) b =
    A.indexedMap
        (\rowIndex row ->
            A.indexedMap
                (\colIndex col ->
                    if rowIndex == y && colIndex == x then
                        Just s

                    else
                        col
                )
                row
        )
        b


type alias Direction =
    ( Int, Int )


checkDirection : Board -> Stone -> Point -> Direction -> Int -> Bool -> Bool
checkDirection b s ( col, row ) ( i, j ) nth res =
    let
        point =
            b |> getPoint ( col, row )
    in
    if Nothing == point then
        False

    else if (res && point == Just s) || (nth == 0 && point == Just s) then
        res

    else
        checkDirection b s ( col + i, row + j ) ( i, j ) (nth + 1) (point == Just (opposite s))


check : Board -> Stone -> Point -> Bool
check b s ( x, y ) =
    let
        directions =
            L.range -1 1
                |> L.map
                    (\i ->
                        L.range -1 1
                            |> L.map
                                (\j ->
                                    if i == 0 && j == 0 then
                                        Nothing

                                    else
                                        Just ( i, j )
                                )
                    )
                |> L.concat
                |> L.filter (\d -> d /= Nothing)
                |> L.map (Maybe.withDefault ( 0, 0 ))

        -- TODO: Ask kevin
    in
    if (b |> getPoint ( x, y )) == Nothing then
        directions
            |> L.map
                (\( i, j ) -> checkDirection b s ( x + i, y + j ) ( i, j ) 0 False)
            |> L.foldl (\val acc -> val || acc) False

    else
        False


turnDirection : Board -> Stone -> Point -> Direction -> Board
turnDirection b s ( col, row ) ( i, j ) =
    let
        point =
            b |> getPoint ( col, row )
    in
    if point == Nothing || point == Just s then
        b

    else
        turnDirection (b |> set s ( col, row )) s ( col + i, row + j ) ( i, j )


turn : Stone -> Point -> Board -> Board
turn s ( x, y ) b =
    let
        directions =
            L.range -1 1
                |> L.map
                    (\i ->
                        L.range -1 1
                            |> L.map
                                (\j ->
                                    if i == 0 && j == 0 then
                                        Nothing

                                    else
                                        Just ( i, j )
                                )
                    )
                |> L.concat
                |> L.filter (\d -> d /= Nothing)
                |> L.map (Maybe.withDefault ( 0, 0 ))
    in
    directions
        |> L.foldl
            (\( i, j ) acc ->
                if checkDirection b s ( x + i, y + j ) ( i, j ) 0 False then
                    turnDirection acc s ( x + i, y + j ) ( i, j )

                else
                    acc
            )
            b


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set s point ->
            if check model.board model.player point then
                ( { model
                    | board = model.board |> set model.player point |> turn model.player point
                    , player = opposite model.player
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


stone : Maybe Stone -> Html Msg
stone s =
    case s of
        Nothing ->
            div [] [ text "" ]

        Just Black ->
            div [ class "w-full m-2 bg-black rounded-full" ] []

        Just White ->
            div [ class "w-full m-2 bg-white rounded-full" ] []


board : Board -> Stone -> Html Msg
board b player =
    div [ class "flex flex-col items-center" ]
        (A.indexedMap
            (\rowIndex row ->
                div [ class "flex" ]
                    (A.indexedMap
                        (\colIndex s ->
                            div
                                [ class "flex w-16 h-16 bg-green-dark border border-black text-white cursor-pointer flex hover:bg-green"
                                , onClick (Set player ( colIndex, rowIndex ))
                                ]
                                [ stone s ]
                        )
                        row
                        |> A.toList
                    )
            )
            b
            |> A.toList
        )


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "flex justify-center" ] [ text "Othello" ]
        , div [ class "flex justfy-center my-2" ]
            [ text
                (case model.player of
                    Black ->
                        "Black"

                    White ->
                        "White"
                )
            ]
        , board model.board model.player
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> { title = "Client", body = [ view m ] }
        , subscriptions = \_ -> Sub.none
        }
