namespace SnakeyConsole
open System
open System.Threading.Tasks

module KeyListener =
    let mutable private keyPressed: (ConsoleKey -> ConsoleKey) = id

    let rec private listen () = async{
        let! key = Async.AwaitTask (Task.Run(fun () -> Console.ReadKey(true).Key))
        keyPressed key |> ignore
        return! listen ()
    }

    let private wrapper f x = 
        f x
        x

    let subscribe listener =
        keyPressed <- (wrapper listener) >> keyPressed
    
    let unsubscribeAll () = keyPressed <- id

    do listen () |> Async.StartImmediate