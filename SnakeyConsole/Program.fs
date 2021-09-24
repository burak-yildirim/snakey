namespace SnakeyConsole

open System
open SnakeEngine.EngineTypes
module Utils = SnakeEngine.Utils

module Main =
    let mutable gameState = Utils.initialState

    let enqueueMove = function
        | ConsoleKey.UpArrow
        | ConsoleKey.W -> gameState <- Utils.enqueue Utils.north gameState
        | ConsoleKey.DownArrow
        | ConsoleKey.S -> gameState <- Utils.enqueue Utils.south gameState
        | ConsoleKey.RightArrow
        | ConsoleKey.D -> gameState <- Utils.enqueue Utils.east gameState
        | ConsoleKey.LeftArrow
        | ConsoleKey.A -> gameState <- Utils.enqueue Utils.west gameState
        | _ -> ()

    let clearGameTable () =
        let cleanStr =
            " "
            |> String.replicate Console.BufferWidth
            |> Seq.replicate Console.BufferHeight
            |> Utils.partial (String.Join) "\n"

        Console.SetCursorPosition(0, 0)
        Console.Write(cleanStr)
        Console.SetCursorPosition(0, 0)
        ()
    
    let rec drawLoop getRandom = async{
        gameState <- Utils.next gameState getRandom
        let map =  gameState |> Table.fromState |> Table.toString
        
        Console.CursorVisible <- false
        Console.SetBufferSize(Console.WindowWidth, Console.WindowHeight)
        clearGameTable ()
        Console.Write(map)
        
        do! Async.Sleep(100)
        return! drawLoop getRandom
    }

    [<EntryPoint>]
    let main argv =
        KeyListener.subscribe enqueueMove
        let getRandom = Utils.partial (Random().Next)
        drawLoop getRandom |> Async.RunSynchronously

        0 // return an integer exit code