module Main exposing (Board, Model, Msg(..), Stone(..), andThen, board, check, init, main, set, stone, update, view)

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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = A.repeat 8 (A.repeat 8 Nothing), player = Black }, Cmd.none )



-- ---------------------------
-- MSG
-- ---------------------------


type Msg
    = Start
    | Set Stone ( Int, Int )



-- ---------------------------
-- UPDATE
-- ---------------------------
-- TODO: Ask kevin about where to put the board params


set : Stone -> ( Int, Int ) -> Board -> Board
set s ( x, y ) b =
    A.indexedMap
        (\i row ->
            A.indexedMap
                (\j col ->
                    if i == x && j == y then
                        Just s

                    else
                        col
                )
                row
        )
        b



-- (x-1, y)
-- (x-1, y-1)
-- (x-1, y+1)
-- (x+1, y)
-- (x+1, y-1)
-- (x+1, y+1)
-- (x, y-1)
-- (x, y+1)
-- case list of
--         [] -> []
--         (x::xs) -> x :: take (n-1) xs


checkDirection2 : Board -> Stone -> ( Int, Int ) -> ( Int, Int ) -> Bool -> Bool
checkDirection2 b s ( x, y ) ( i, j ) res =
    let
        booo =
            Debug.log "board" b

        a =
            Debug.log "x" x

        c =
            Debug.log "y" y

        i2 =
            Debug.log "i" i

        j2 =
            Debug.log "j" j

        pointB =
            Debug.log "hi"
                (A.get x b
                    |> Maybe.withDefault ([] |> A.fromList)
                )

        point =
            Debug.log "hi22"
                (pointB
                    |> A.get y
                    |> Maybe.withDefault Nothing
                )
    in
    if Nothing == point then
        res

    else
        checkDirection2 b s ( x + i, x + j ) ( i, j ) (point == Just (opposite s))


checkDirection : Board -> Stone -> ( Int, Int ) -> ( Int, Int ) -> Bool
checkDirection b s ( x, y ) ( i, j ) =
    let
        yo =
            A.foldl
                (\val acc ->
                    let
                        point =
                            A.get (x + (i * val)) b
                                |> Maybe.withDefault ([] |> A.fromList)
                                |> A.get (y + (j * val))
                                |> Maybe.withDefault Nothing
                    in
                    acc || (val /= 0 && point == Just (opposite s)) || (val == 0 && point == Nothing)
                )
                True
                (A.initialize 8 identity)
    in
    True


check : Board -> Stone -> ( Int, Int ) -> Bool
check b s ( x, y ) =
    let
        -- a =
        --     L.map (\i -> L.map (\j -> checkDirection2 b s ( x, y ) ( i, j ) False) (L.range -1 1)) (L.range -1 1)
        a =
            checkDirection2 b s ( x, y ) ( -1, 0 ) False
    in
    a


andThen : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen msg ( model, cmd ) =
    let
        ( newmodel, newcmd ) =
            update msg model
    in
    ( newmodel, Cmd.batch [ cmd, newcmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            update (Set White ( 3, 3 )) model
                |> andThen (Set White ( 4, 4 ))
                |> andThen (Set Black ( 4, 3 ))
                |> andThen (Set Black ( 3, 4 ))
                |> andThen (Set Black ( 5, 4 ))

        Set s point ->
            let
                swap =
                    case model.player of
                        Black ->
                            White

                        White ->
                            Black
            in
            ( { model | board = model.board |> set s point, player = swap }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


stone : Maybe Stone -> Html Msg
stone s =
    case s of
        Nothing ->
            div [] [ text "-" ]

        Just Black ->
            div [ class "w-full m-2 bg-black rounded-full" ] []

        Just White ->
            div [ class "w-full m-2 bg-white rounded-full" ] []


board : Board -> Stone -> Html Msg
board b player =
    div [ class "flex flex-col items-center" ]
        (A.indexedMap
            (\i row ->
                div [ class "flex" ]
                    (A.indexedMap
                        (\j s ->
                            div
                                [ class "flex w-16 h-16 bg-green-dark border border-black text-white cursor-pointer flex hover:bg-green"
                                , onClick (Set player ( i, j ))
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
        [ h1 [] [ text "Othello" ]
        , div []
            [ text
                (case model.player of
                    Black ->
                        "Black"

                    White ->
                        "White"
                )
            ]
        , board model.board model.player
        , button [ onClick Start ] [ text "start" ]
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
