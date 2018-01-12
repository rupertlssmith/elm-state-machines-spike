module Game exposing (..)

import Html exposing (Html)
import Task
import GameState exposing (..)


-- An example of it in action.


type Msg
    = Loaded GameDefinition
    | StartGame
    | Die Int
    | AnotherGo


type alias Model =
    { game : Game
    , previous : List Game
    , count : Int
    }


message msg =
    Task.perform identity (Task.succeed msg)


main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { game = loading
      , previous = []
      , count = 5
      }
    , message <| Loaded { boardSize = 100 }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )

        ( nextGame, cmd ) =
            case ( model.game, (Debug.log "update" msg) ) of
                ( Loading state loading, Loaded gameDefinition ) ->
                    ( { model | game = toReady state gameDefinition }
                    , message StartGame
                    )

                ( Ready state ready, StartGame ) ->
                    ( { model | game = toInPlay state ready { score = 0, position = [] } }
                    , message <| Die 123
                    )

                ( InPlay state inPlay, Die finalScore ) ->
                    ( { model | game = toGameOver state <| (updatePlayState <| updateScore finalScore) inPlay }
                    , message AnotherGo
                    )

                ( GameOver state gameOver, AnotherGo ) ->
                    ( { model | game = toReady state gameOver.definition }
                    , message StartGame
                    )

                ( _, _ ) ->
                    noop
    in
        if model.count > 0 then
            ( { nextGame
                | previous = model.game :: model.previous
                , count = model.count - 1
              }
            , cmd
            )
        else
            noop


view : Model -> Html Msg
view model =
    Html.div [] <|
        List.map (\game -> Html.p [] [ Html.text (toString game) ]) (List.reverse model.previous)
