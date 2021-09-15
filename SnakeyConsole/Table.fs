namespace SnakeyConsole

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
