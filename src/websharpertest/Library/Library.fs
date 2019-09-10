[<WebSharper.JavaScriptExport>]
module Library

open WebSharper.JavaScript

let startWorker() =

    //navigator.hardwareConcurrency

    //https://developers.websharper.com/docs/v4.x/fs/web-workers

    let myWorker = new Worker(fun self ->

        self.Onmessage <- fun event ->
            Console.Log "This was written from the worker!"

            let utility = LibraryCS.Utility()
            utility.Multiply(3) |> Console.Log
            ()

        //self.PostMessage("This worker's job is done, it can be terminated.")
    )


    myWorker.PostMessage(3)

    myWorker.Onmessage <- fun event ->
        Console.Log event.Data

        if event.Data.ToString() = "This worker's job is done, it can be terminated." then
            myWorker.Terminate()