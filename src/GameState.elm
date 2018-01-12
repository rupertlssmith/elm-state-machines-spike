module GameState
    exposing
        ( Game(..)
        , map
        , untag
        , GameDefinition
        , PlayState
        , loading
        , updatePlayState
        , updateScore
        , toReady
        , toReadyWithGameDefinition
        , toInPlayWithPlayState
        , toGameOver
        )

-- Reusable state machine concepts.


type Allowed
    = Allowed


type State trans model
    = State model



-- Permitted operations on State that do not allow arbitrary states to be
-- constructed in order to bypass the type checking on state transitions.


map : (a -> b) -> State tag a -> State tag b
map f (State x) =
    State (f x)


untag : State tag value -> value
untag (State x) =
    x



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


updateScore : Int -> PlayState -> PlayState
updateScore score play =
    { play | score = score }


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


toInPlayWithPlayState : PlayState -> State { a | inPlay : Allowed } { m | definition : GameDefinition } -> Game
toInPlayWithPlayState play (State model) =
    inPlay model.definition play


toGameOver : State { a | gameOver : Allowed } { m | definition : GameDefinition, play : PlayState } -> Game
toGameOver (State model) =
    gameOver model.definition model.play.score
