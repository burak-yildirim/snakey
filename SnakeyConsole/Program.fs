namespace SnakeyConsole

open System
open SnakeEngine.EngineTypes
open System.Threading.Tasks
module Utils = SnakeEngine.Utils

module Main =
    let rec listenKeys func = async{
        let! key = Async.AwaitTask (Task.Run(fun () -> Console.ReadKey(true).Key))
        func key
        return! listenKeys func
    }

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

    let rec drawLoop getRandom = async{
        gameState <- Utils.next gameState getRandom
        let map =  gameState |> Table.fromState |> Table.toString
        Console.Clear()
        Console.Write(map)
        do! Async.Sleep(100)
        return! drawLoop getRandom
    }

    [<EntryPoint>]
    let main argv =
        let getRandom = Utils.partial (Random().Next)
        [listenKeys enqueueMove; drawLoop getRandom]
        |> Async.Parallel 
        |> Async.RunSynchronously 
        |> ignore

        0 // return an integer exit code