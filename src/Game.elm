module Game exposing (..)

import Html exposing (Html)
import Task


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


msgToCmd msg =
    Task.perform (\() -> msg) (Task.succeed ())


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
    , msgToCmd <| Loaded { boardSize = 100 }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )

        ( nextGame, cmd ) =
            case ( model.game, (Debug.log "update" msg) ) of
                ( Loading loading, Loaded gameDefinition ) ->
                    ( { model | game = toReadyWithGameDefinition gameDefinition loading }
                    , msgToCmd StartGame
                    )

                ( Ready ready, StartGame ) ->
                    ( { model | game = toInPlay { score = 0, position = [] } ready }
                    , msgToCmd <| Die 123
                    )

                ( InPlay inPlay, Die finalScore ) ->
                    ( { model | game = toGameOver <| (updatePlayState <| updateScore finalScore) inPlay }
                    , msgToCmd AnotherGo
                    )

                ( GameOver gameOver, AnotherGo ) ->
                    ( { model | game = toReady gameOver }
                    , msgToCmd StartGame
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



-- Reusable state machine concepts.


type Allowed
    = Allowed


type State trans model
    = State model



-- An Example model for a game of some kind.


type alias GameDefinition =
    { boardSize : Int
    }


type alias PlayState =
    { score : Int
    , position : List Int
    }


updateScore : Int -> PlayState -> PlayState
updateScore score play =
    { play | score = score }


type Game
    = Loading Loading
    | Ready Ready
    | InPlay InPlay
    | GameOver GameOver



-- The state definitions with enough typing information to enforce matching
-- states against legal state transitions, and against the available data model
-- in the state.


type alias Loading =
    State { ready : Allowed } {}


type alias Ready =
    State { inPlay : Allowed } { definition : GameDefinition }


type alias InPlay =
    State { gameOver : Allowed } { definition : GameDefinition, play : PlayState }


type alias GameOver =
    State { ready : Allowed } { definition : GameDefinition, finalScore : Int }



-- State constructors.


loading : Game
loading =
    State {} |> Loading


ready : GameDefinition -> Game
ready definition =
    State { definition = definition } |> Ready


inPlay : GameDefinition -> PlayState -> Game
inPlay definition play =
    State { definition = definition, play = play } |> InPlay


gameOver : GameDefinition -> Int -> Game
gameOver definition score =
    State { definition = definition, finalScore = score } |> GameOver



-- Map functions that can be applied when parts of the model are present.


mapGameDefinition : (GameDefinition -> a) -> State p { m | definition : GameDefinition } -> a
mapGameDefinition func (State model) =
    func model.definition



-- ... more mapping functions
-- Update functions that can be applied when parts of the model are present.


updateGameDefinition :
    (GameDefinition -> GameDefinition)
    -> State p { m | definition : GameDefinition }
    -> State p { m | definition : GameDefinition }
updateGameDefinition func (State model) =
    State { model | definition = func model.definition }


updatePlayState :
    (PlayState -> PlayState)
    -> State p { m | play : PlayState }
    -> State p { m | play : PlayState }
updatePlayState func (State model) =
    State { model | play = func model.play }



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toReady : State { a | ready : Allowed } { m | definition : GameDefinition } -> Game
toReady (State model) =
    ready model.definition


toReadyWithGameDefinition : GameDefinition -> State { a | ready : Allowed } m -> Game
toReadyWithGameDefinition definition game =
    ready definition


toInPlay : PlayState -> State { a | inPlay : Allowed } { m | definition : GameDefinition } -> Game
toInPlay play (State model) =
    inPlay model.definition play


toGameOver : State { a | gameOver : Allowed } { m | definition : GameDefinition, play : PlayState } -> Game
toGameOver (State model) =
    gameOver model.definition model.play.score
