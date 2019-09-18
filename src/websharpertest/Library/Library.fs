[<WebSharper.JavaScriptExport>]
module Library

open WebSharper.JavaScript

let startWorker (callback:string->unit) =
    //https://developers.websharper.com/docs/v4.x/fs/web-workers
    let procceses:int = JS.Inline("navigator.hardwareConcurrency")
    for i in 0..(procceses*3) do
        let worker = new Worker(fun self ->
            self.Onmessage <- fun event ->
                let data = event.Data :?> int
                let utility = LibraryCS.Utility()
                let result = utility.Process(data)
                self.PostMessage(result)
        )
        worker.PostMessage(i)
        worker.Onmessage <- fun event ->
            callback (sprintf "Worker ID=%i Result=%O" i event.Data)
            worker.Terminate()