module GameState exposing (..)

-- Reusable state machine concepts.


type Allowed
    = Allowed


type State trans
    = State



-- An Example model for a game of some kind.


type alias GameDefinition =
    { boardSize : Int
    }


type alias PlayState =
    { score : Int
    , position : List Int
    }


type Game
    = Loading Loading {}
    | Ready Ready { definition : GameDefinition }
    | InPlay InPlay { definition : GameDefinition, play : PlayState }
    | GameOver GameOver { definition : GameDefinition, finalScore : Int }



-- The state definitions with enough typing information to enforce matching
-- states against legal state transitions, and against the available data model
-- in the state.


type alias Loading =
    State { ready : Allowed }


type alias Ready =
    State { inPlay : Allowed }


type alias InPlay =
    State { gameOver : Allowed }


type alias GameOver =
    State { ready : Allowed }



-- State constructors.


{-| The initial state. Only this constructor should be exposed.
-}
loading : Game
loading =
    Loading State {}


ready : GameDefinition -> Game
ready definition =
    Ready State { definition = definition }


inPlay : GameDefinition -> PlayState -> Game
inPlay definition play =
    InPlay State { definition = definition, play = play }


gameOver : GameDefinition -> Int -> Game
gameOver definition score =
    GameOver State { definition = definition, finalScore = score }



-- Update functions that can be applied when parts of the model are present.


updateScore : Int -> PlayState -> PlayState
updateScore score play =
    { play | score = score }


updateGameDefinition :
    (GameDefinition -> GameDefinition)
    -> { m | definition : GameDefinition }
    -> { m | definition : GameDefinition }
updateGameDefinition func model =
    { model | definition = func model.definition }


updatePlayState :
    (PlayState -> PlayState)
    -> { m | play : PlayState }
    -> { m | play : PlayState }
updatePlayState func model =
    { model | play = func model.play }



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toReady : State { a | ready : Allowed } -> GameDefinition -> Game
toReady _ definition =
    ready definition


toInPlay : State { a | inPlay : Allowed } -> { m | definition : GameDefinition } -> PlayState -> Game
toInPlay _ model play =
    inPlay model.definition play


toGameOver : State { a | gameOver : Allowed } -> { m | definition : GameDefinition, play : PlayState } -> Game
toGameOver _ model =
    gameOver model.definition model.play.score
