open System
open SnakeEngine.EngineTypes
module Utils = SnakeEngine.Utils

type Table = string list list
module Table =
    let make state : Table =
        " . "
        |> List.replicate state.Cols
        |> List.replicate state.Rows
    
    let set value point : (Table -> Table) =
        Utils.always value
        |> Utils.adjust point.X
        |> Utils.adjust point.Y

    let drawSnake state =
        state.Snake
        |> List.map (set " X ")
        // we use fold and id in case of empty list
        |> List.fold (>>) id

    let drawApple state = set " o " state.Apple

    let drawCrash state : (Table -> Table)=
        if List.isEmpty state.Snake then
            Utils.always " # " |> List.map |> List.map
        else
            id

    let toString (table: Table) =
        table
        |> List.map (Utils.strJoin "")
        |> Utils.strJoin "\n\r"
    
    let fromState state =
        state
        |> make
        |> drawSnake state
        |> drawApple state
        |> drawCrash state

let rec gameLoop time state getRandom =
    let enqueuedState =
        if not Console.KeyAvailable then
            state
        else
            let key = Console.ReadKey().Key
            match key with
            | ConsoleKey.UpArrow
            | ConsoleKey.W -> Utils.enqueue Utils.north state
            | ConsoleKey.DownArrow
            | ConsoleKey.S -> Utils.enqueue Utils.south state
            | ConsoleKey.RightArrow
            | ConsoleKey.D -> Utils.enqueue Utils.east state
            | ConsoleKey.LeftArrow
            | ConsoleKey.A -> Utils.enqueue Utils.west state
            | _ -> state

    let newTime = Utils.epoch ()
    if newTime - time > 80L then
        let newState = Utils.next enqueuedState getRandom
        let map =  newState |> Table.fromState |> Table.toString
        Console.Clear()
        Console.Write(map)
        gameLoop newTime newState getRandom
    else
        gameLoop time enqueuedState getRandom

[<EntryPoint>]
let main argv =
    let time = Utils.epoch ()
    let state = Utils.initialState
    let getRandom = Utils.partial (Random().Next)
    gameLoop time state getRandom |> ignore
    0 // return an integer exit code