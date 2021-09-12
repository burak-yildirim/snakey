namespace SnakeEngine
open System
open System.Collections.Generic

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
    
    let safeTail list =
        match list with
        | [] -> []
        | l -> List.tail l
    
    let rec modulo x y =
        if x < 0 then
            modulo (x + y) y
        else
            x % y
    
    let nextHead state = 
        if List.isEmpty state.Snake then
            { X = 2; Y = 2}
        else
            let { X = snakeX; Y = snakeY } = 
                List.last state.Snake
            let { X = moveX; Y = moveY } =
                List.last state.Moves
            
            { X = modulo (snakeX + moveX) state.Cols
              Y = modulo (snakeY + moveY) state.Rows }
    
    let willEat state = (nextHead state) = state.Apple
    
    let willCrash state =
        let nh = nextHead state
        List.exists (fun x -> x = nh) state.Snake
    
    let isValidMove move state =
        match state.Moves with
        | [] -> true // this should never be the case as state.Moves shouldn't be empty
        | lastToApply :: _ ->
            lastToApply.X + move.X <> 0 || lastToApply.Y + move.Y <> 0

    let nextMoves state =
        if List.length state.Moves > 1 then
            List.tail state.Moves
        else
            state.Moves
    
    let nextApple state getRandom =
        if willEat state then
            { X = getRandom 0 state.Cols; Y = getRandom 0 state.Rows }
        else if willCrash state then
            { X = 16; Y = 2 }
        else
            state.Apple

    let nextSnake state =
        if willCrash state then
            []
        else if willEat state then
            List.append state.Snake [nextHead state]
        else 
            List.append (safeTail state.Snake) [nextHead state]

    let next currentState getRandom : GameState =
        { Cols = currentState.Cols
          Rows = currentState.Rows
          Moves = nextMoves currentState
          Snake = nextSnake currentState
          Apple = nextApple currentState getRandom }
    
    let enqueue move state =
        if isValidMove move state then
            let newMoves = List.append state.Moves [move]
            { state with Moves = newMoves }
        else
            state
    
    let adjust index (f: 'a -> 'a) (aList: 'a list) =
         List.mapi (fun i x -> if index = i then (f x) else x) aList
    let always (x: 'a) (y: 'b) = x
    let strJoin seperator (sList: IEnumerable<string>) = String.Join(seperator, sList)
    let epoch () = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
    let partial f x y = f(x, y)