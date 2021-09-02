namespace SnakeEngine

module EngineTypes =
    type Point = { X: int; Y: int }
    type GameState = 
        { Cols: int
          Rows: int
          Moves: Point list
          Snake: Point list
          Apple: Point }

module Utils = 
    open EngineTypes

    let north = { X = 0; Y = -1 }
    let south = { X = 0; Y = 1 }
    let east = { X = 1; Y = 0 }
    let west = { X = -1; Y = 0 }

    let initialState = 
        { Cols = 20
          Rows = 14
          Moves = [east]
          Snake = []
          Apple = { X = 16; Y = 2} }
    
    let pointEq (p1: Point) (p2: Point) = p1 = p2

    let nextHead (state: GameState) = 
        if List.isEmpty state.Snake then
            { X = 2; Y = 2}
        else
            let { X = snakeX; Y = snakeY } = 
                List.head state.Snake
            let { X = moveX; Y = moveY } =
                List.head state.Moves
            
            { X = (snakeX + moveX) % state.Cols
              Y = (snakeY + moveY) % state.Rows }
    
    let willEat state = (nextHead state) = state.Apple
    
    let willCrash state =
        let nh = nextHead state
        List.exists (fun x -> x = nh) state.Snake
    
    let isValidMove move state =
        match state.Moves with
        | [] -> true // this should never be the case as state.Moves shouldn't be empty
        | lastMove :: _ ->
            lastMove.X + move.X <> 0 || lastMove.Y + move.Y <> 0

    let nextMoves state =
        if List.length state.Moves > 1 then
            List.tail state.Moves
        else
            state.Moves
    
    // let nextApple

    let next (old: GameState) : GameState =
        { Cols = old.Cols
          Rows = old.Rows
          Moves = []
          Snake = []
          Apple = north }
