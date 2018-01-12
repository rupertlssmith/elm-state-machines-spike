module GameState
    exposing
        ( Game(..)
        , GameDefinition
        , PlayState
        , loading
        , updateGameDefinition
        , updatePlayState
        , updateScore
        , toReady
        , toReadyWithGameDefinition
        , toInPlayWithPlayState
        , toGameOver
        )

import StateMachine exposing (..)


-- An Example model for a game of some kind.


type alias GameDefinition =
    { boardSize : Int
    }


type alias PlayState =
    { score : Int
    , position : List Int
    }


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



-- Update functions that can be applied when parts of the model are present.


mapDefinition : (a -> b) -> ({ m | definition : a } -> { m | definition : b })
mapDefinition func =
    \model -> { model | definition = func model.definition }


mapPlay : (a -> b) -> ({ m | play : a } -> { m | play : b })
mapPlay func =
    \model -> { model | play = func model.play }


updateGameDefinition :
    (GameDefinition -> GameDefinition)
    -> State p { m | definition : GameDefinition }
    -> State p { m | definition : GameDefinition }
updateGameDefinition func state =
    map (mapDefinition func) state


updatePlayState :
    (PlayState -> PlayState)
    -> State p { m | play : PlayState }
    -> State p { m | play : PlayState }
updatePlayState func state =
    map (mapPlay func) state


updateScore : Int -> PlayState -> PlayState
updateScore score play =
    { play | score = score }



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toReady : State { a | ready : Allowed } { m | definition : GameDefinition } -> Game
toReady (State model) =
    ready model.definition


toReadyWithGameDefinition : GameDefinition -> State { a | ready : Allowed } m -> Game
toReadyWithGameDefinition definition game =
    ready definition


toInPlayWithPlayState : PlayState -> State { a | inPlay : Allowed } { m | definition : GameDefinition } -> Game
toInPlayWithPlayState play (State model) =
    inPlay model.definition play


toGameOver : State { a | gameOver : Allowed } { m | definition : GameDefinition, play : PlayState } -> Game
toGameOver (State model) =
    gameOver model.definition model.play.score
