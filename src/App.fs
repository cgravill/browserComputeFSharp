module App

open Elmish
open Elmish.React
open Elmish.Navigation
open Elmish.UrlParser
open Fable.React
open Fable.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Browser.Blob
open Browser.Types
open Fulma

open Monaco

open Elmish.HMR

type Model = {
  count: int64
  enteredPage: bool
  page: int
  primeFactors: int64[]
  outputs: List<string>
}

type Msg =
| NextPage
| PreviousPage
| Increment
| Decrement
| MassiveCalculation
| ExpensiveCalculationAsync
| ForceCounterTo of int64
| ComputedPrimeFactors of int64[]
| UpdatedOutputs of string

//Quick way to do pages
let page =
  if isNull Browser.Dom.window.location.hash || Browser.Dom.window.location.hash.Length <= 1 then 
    0
  else
    Browser.Dom.window.location.hash.[1..] |> int

let init() : Model =
  {count=0L; enteredPage=false; page=page; primeFactors=[||]; outputs=[]}

let maxPage = 50

let update (msg:Msg) (model:Model) =

    (*let adjustCountForDramaticalReasons page =
        match page with
        | 0 -> 0L
        | 2 -> 12L
        | 3 -> 3349L
        | 4 -> 5029784645645674576L
        | _ -> model.count*)

    match msg with
    | NextPage ->
      let newPage = min maxPage (model.page + 1)
      
      Browser.Dom.history.replaceState((), "", sprintf "#%i" newPage)
      {model with enteredPage = true; page = newPage; primeFactors = [||]; outputs = [] }
    | PreviousPage ->
      let newPage = max 0 (model.page - 1)
      Browser.Dom.history.replaceState((), "", sprintf "#%i" newPage)
      {model with enteredPage = true; page = newPage; primeFactors = [||]; outputs = [] }
    | Increment -> {model with enteredPage = false; count = model.count + 1L }
    | Decrement -> {model with enteredPage = false; count = model.count - 1L }
    | MassiveCalculation ->
      model
    | ExpensiveCalculationAsync ->
      model
    | ComputedPrimeFactors factors ->
      {model with primeFactors = factors}
    | UpdatedOutputs text ->
      {model with outputs = text :: model.outputs}
    | ForceCounterTo value ->
      {model with count = value}

type IActualModule =
  abstract isAwesome: unit -> bool

//Better option: https://math.stackexchange.com/questions/185524/pollard-strassen-algorithm
let factorise n =
  let rec f number candidate acc = 
    if candidate = number then
        candidate::acc
    elif number % candidate = 0L then 
        f (number/candidate) candidate (candidate::acc)
    else
        f number (candidate+1L) acc
  f n 2L []
let factors (count:int64) = factorise count |> Array.ofList

let primeFactors count dispatch =
  dispatch (ComputedPrimeFactors (factors count))

let expensiveCalculationCode = """let expensiveCalculation dispatch =
  JS.console.time("calc")
  for i in 0L..20000000L do
    if i % 20000L = 0L then
      dispatch (UpdatedOutputs (string i))
      JS.console.log(i)
  JS.console.timeEnd("calc")
  dispatch MassiveCalculation"""

let expensiveCalculation dispatch =
  JS.console.time("calc")
  for i in 0L..20000000L do
    if i % 20000L = 0L then
      dispatch (UpdatedOutputs (string i))
      JS.console.log(i)
  JS.console.timeEnd("calc")
  dispatch MassiveCalculation

let expensiveCalculationAsyncCode = """let expensiveCalculationAsync dispatch =
  async {
    for i in 0L..20000000L do
      if i % 20000L = 0L then
        dispatch (UpdatedOutputs (string i))
    dispatch ExpensiveCalculationAsync
  }
  |> Async.StartImmediate"""

let expensiveCalculationAsync dispatch =
  async {
    for i in 0L..20000000L do
      if i % 20000L = 0L then
        dispatch (UpdatedOutputs (string i))
    dispatch ExpensiveCalculationAsync
  }
  |> Async.StartImmediate

let [<Global>] URL: obj = jsNative

let expensiveCalculationWorkerCode = """let expensiveCalculationWorker dispatch =

  //Needs to be self-contained, or otherwise arrange for called functions to be present via ImportScripts etc.
  let start() =
    self.onmessage <-
      fun e -> 
        self.postMessage("WorkerX: " + (string)e.data)
        for i = 0 to 20000000 do
          if i % 20000 = 0 then
            self.postMessage(i)

  //https://stackoverflow.com/questions/10343913/how-to-create-a-web-worker-from-a-string/10372280#10372280
  //https://github.com/fable-compiler/repl/blob/master/src/App/Generator.fs#L107
  let asString = start.ToString() + System.Environment.NewLine + "start();"
  let parts: obj[] = [| asString |]
  
  let options =
      JsInterop.jsOptions<Browser.Types.BlobPropertyBag>(fun o ->
          o.``type`` <- "text/javascript")

  let blobUrl = URL?createObjectURL(Blob.Create(parts, options))
  let worker = Browser.Dom.Worker.Create(blobUrl)

  let workerCallback (ev:Browser.Types.MessageEvent) =
    dispatch (UpdatedOutputs (string ev.data))

  worker.onmessage <- workerCallback
  worker.postMessage("")"""

let [<Global>] self: Browser.Types.Worker = jsNative

let expensiveCalculationWorker dispatch =

  //Needs to be self-contained, or otherwise arrange for called functions to be present via ImportScripts etc.
  let start() =
    self.onmessage <-
      fun e -> 
        self.postMessage("WorkerX: " + (string)e.data)
        for i = 0 to 20000000 do
          if i % 20000 = 0 then
            self.postMessage(i)

  //https://stackoverflow.com/questions/10343913/how-to-create-a-web-worker-from-a-string/10372280#10372280
  //https://github.com/fable-compiler/repl/blob/master/src/App/Generator.fs#L107
  let asString = start.ToString() + System.Environment.NewLine + "start();"
  let parts: obj[] = [| asString |]
  
  let options =
      JsInterop.jsOptions<Browser.Types.BlobPropertyBag>(fun o ->
          o.``type`` <- "text/javascript")

  let blobUrl = URL?createObjectURL(Blob.Create(parts, options))
  let worker = Browser.Dom.Worker.Create(blobUrl)

  let workerCallback (ev:Browser.Types.MessageEvent) =
    dispatch (UpdatedOutputs (string ev.data))

  worker.onmessage <- workerCallback
  worker.postMessage("")

let doExpensiveCalculationWasmCode = """[<Import("default", @"./wasm/fibonacci.js")>]
let FibonacciModule :unit -> JS.Promise<IActualModule> =
  jsNative
let doExpensiveCalculationWasm dispatch =

  (*import Module from './fibonacci.js'
    Module().then(function(mymod) {
        const fib = mymod.cwrap('fib', 'number', ['number']);
        console.log(fib(64));
    });*)

  FibonacciModule().``then``(fun fibonacciModule ->
    let fib = fibonacciModule?cwrap("fib", "number", ["number"])
    dispatch (UpdatedOutputs ("fibonacci(64)=" + string (fib(64)))))
  |> ignore"""

[<Import("default", @"./wasm/fibonacci.js")>]
let FibonacciModule :unit -> JS.Promise<IActualModule> =
  jsNative
let doExpensiveCalculationWasm dispatch =

  (*import Module from './fibonacci.js'
    Module().then(function(mymod) {
        const fib = mymod.cwrap('fib', 'number', ['number']);
        console.log(fib(64));
    });*)

  FibonacciModule().``then``(fun fibonacciModule ->
    let fib = fibonacciModule?cwrap("fib", "number", ["number"])
    dispatch (UpdatedOutputs ("fibonacci(64)=" + string (fib(64)))))
  |> ignore

let doExpensiveCalculationWasmCppCode = """#include <emscripten.h>

//https://emscripten.org/docs/porting/connecting_cpp_and_javascript/Interacting-with-code.html#calling-compiled-c-functions-from-javascript-using-ccall-cwrap
EMSCRIPTEN_KEEPALIVE
int fib(int n) {
  int i, t, a = 0, b = 1;
  for (i = 0; i < n; i++) {
    t = a + b;
    a = b;
    b = t;
  }
  return b;
}"""

let energyCalculationCode = """[<Import("default", @"./wasm/dna.js")>]
let DNAModule :unit -> JS.Promise<IActualModule> = jsNative

let energyCaclulation dispatch =

  DNAModule().``then``(fun energyModule ->
    JS.console.log(energyModule?energyWrapped("GACCTTACC")))
  |> ignore"""

[<Import("default", @"./wasm/dna.js")>]
let DNAModule :unit -> JS.Promise<IActualModule> = jsNative

let energyCaclulation dispatch =

  DNAModule().``then``(fun energyModule ->
    dispatch (UpdatedOutputs (sprintf "energy(DNA sequence)=%OJ" (energyModule?energyWrapped("GACCTTACC"))))
  ) |> ignore

let energyCalculationCppCode = """#include <emscripten.h>
#include <emscripten/bind.h>

#include <string>

using namespace emscripten;

EMSCRIPTEN_KEEPALIVE
int energy(std::string inStr) {
    return inStr.length();
}

//https://emscripten.org/docs/porting/connecting_cpp_and_javascript/embind.html#embind
EMSCRIPTEN_BINDINGS(my_module) {
    function("energyWrapped", &energy);
}"""

let private fsharpEditorOptions language (fontSize : float) (fontFamily : string) =
  jsOptions<Monaco.Editor.IEditorConstructionOptions>(fun o ->
      let minimapOptions = jsOptions<Monaco.Editor.IEditorMinimapOptions>(fun oMinimap ->
          oMinimap.enabled <- Some false
      )
      o.language <- Some language
      o.fontSize <- Some fontSize
      o.theme <- Some "vs-dark"
      o.minimap <- Some minimapOptions
      o.fontFamily <- Some fontFamily
      o.fontLigatures <- Some (fontFamily = "Fira Code")
      o.fixedOverflowWidgets <- Some true
  )

let xEditor language model dispatch code =
  div
      [ Style [Height "500px"] ]
      [
        ReactEditor.editor [
          ReactEditor.Options (fsharpEditorOptions language 24.0 "Fira Code")
          ReactEditor.Value code
        ]
    ]

let fsharpEditor = xEditor "fsharp"
let cppEditor = xEditor "cpp"
let csharpEditor = xEditor "csharp"
let javaScriptEditor = xEditor "javascript"

let page0 (model:Model) dispatch =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h1 [ ]
                [ str "Browser-based intensive computation with F#" ]
              Heading.h2 [ Heading.IsSubtitle ]
                [ str "Colin Gravill (@cgravill)" ] ]
        ]
    ]

let pagePlaceHolder text1 text2 (model:Model) dispatch =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h1 [ ]
                [ str text1 ]
              Heading.h2 [ Heading.IsSubtitle ]
                [ str text2 ] ]
        ]
    ]

let pageImage text1 imageUrl (model:Model) dispatch =

  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [
          Container.container
            [
              //Container.IsFluid
              
              //Container.IsFullHD
              Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
            ]
            [
              Heading.h1 [ ]
                [ str text1 ]
              Image.image
                [
                  //Image.Option.CustomClass "IsInlineBlock"
                  Image.Is3by2
                  //Image.Props [Style [MaxHeight "950px"]]
                ]
                [ img
                    [
                      Src imageUrl
                      //Style [MaxHeight "950px"]
                    ]
                ]
            ]
        ]
    ]

let pageApplication (model:Model) dispatch =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [
              Heading.h1 [ ]
                [ str "Immune system reprogramming" ]


              Columns.columns 
                [  ]
                [
                  Column.column
                    [ ]
                    [
                      Image.image
                        [
                          Image.Is128x128
                          Image.Option.CustomClass "IsInlineBlock"
                        ]
                        [ img [ Src "images/oxbLogo.png" ] ]

                      Image.image
                        [
                          
                          Image.Option.CustomClass "IsInlineBlock"
                        ]
                        [ img [ Src "images/bbcLivingDrug.png" ] ]
                    ]
                  Column.column [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left)] ]
                    [
                        Heading.h2 [] [str "Kymriah"]
                        h2 [] [str "First FDA approved CAR-T, with Novartis."]
                        h2 [] [str "81% of terminally ill patients in remission after treatment"]
                        str "Costs nearly $0.5M per patient with ALL"

                        br []
                        br []
                        br []

                        Heading.h2 [] [str "We are helping"]
                        h2 [] [str  "Improve yield and quality of lentiviral vector, produced by HEK293T cells"]
                        h2 [] [str  "Optimizing transfection and bioreactor yields"]
                        h2 [] [str  "Facilitate future production of new therapies"]

                    
                    
                    ]
                ]

              
            ]
        ]
    ]

let pageInferenceVideo (model:Model) dispatch =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [
              Heading.h1 [ ]
                [ str "Video" ]

              video [ Src "images/inference.mp4"; AutoPlay true ] []
            ]
        ]
    ]
let sampleApplication (count:int64) dispatch =
  div
    []
    [
      br []

      div
        []
        [ 
          Button.button
            [ Button.Props [OnClick (fun _ -> dispatch Increment)] ]
            [ str "+" ]
          div [] [ str (string count) ]
          Button.button
            [ Button.Props [OnClick (fun _ -> dispatch Decrement)] ]
            [ str "-" ]

        ]
    ]

let pageTitle (model:Model) dispatch =

  if (model.count <> 0L && model.enteredPage) then
    dispatch (ForceCounterTo 0L)

  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            
            [
              Heading.h1
                []
                [str "let's start with something simple:"]
              
              sampleApplication model.count dispatch]
        
        ]
    ]



let pageCounterGeneral forceCounterNumberDrama (model:Model) dispatch  =

  if (model.count <> forceCounterNumberDrama && model.enteredPage) then
    dispatch (ForceCounterTo forceCounterNumberDrama)

  let contents =
    [
      sampleApplication model.count dispatch 
      br []
      Button.button
        [ Button.Props [OnClick (fun _ -> primeFactors model.count dispatch)] ]
        [ str "Prime factors" ]

      h1
        []
        [str (string model.primeFactors)]
    ]

  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]

            contents
        ]
    ]

let pageCounter1 = pageCounterGeneral 12L
let pageCounter2 = pageCounterGeneral 3349L
let pageCounter3 = pageCounterGeneral 5029784645645674576L

let pageGeneral code dispatchFunc title text (model:Model) dispatch =
  let outputs =
    model.outputs
    |> List.rev
    |> String.concat "\n"
  let content =
    [
      br []
      h1 [] [str title]
      sampleApplication model.count dispatch
      br []
      div
        []
        [
          fsharpEditor model dispatch code

          Button.button
            [ Button.Props [OnClick (fun _ -> dispatchFunc dispatch)] ]
            [ str text ]

          pre
              []
              [str (outputs.ToString())]
        ]
    ]

  Container.container [ Container.IsFluid ]
    [
      Content.content
        []
        content
    ]

let pageGeneralTwoColumn editor1 editor2 dispatchFunc title text (model:Model) dispatch =
  let outputs =
    model.outputs
    |> List.rev
    |> String.concat "\n"
  let content =
    [
      br []
      h1 [] [str title]
      sampleApplication model.count dispatch
      br []
      div
        []
        [
          //replace with columns...
          div
            [Style [CSSProp.Width "50%"]]
            [
              
            ]

          Columns.columns
            [ ]
            [
              Column.column [ ]
                [ 
                  editor1
                ]
              Column.column [ ]
                [
                  editor2
                ]
            ]
          

          Button.button
            [ Button.Props [OnClick (fun _ -> dispatchFunc dispatch)] ]
            [ str text ]

          pre
              []
              [str (outputs.ToString())]
        ]
    ]

  Container.container [ Container.IsFluid ]
    [
      Content.content
        []
        content
    ]

let pageOptimiseIt (model:Model) dispatch =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h1 [ ]
                [ str "So just write faster code already?" ]]
        ]
    ]

let pageExpensiveCalculation = pageGeneral expensiveCalculationCode expensiveCalculation "Synthetic problem" "Expensive calculation"
let pageExpensiveCalculationAsync = pageGeneral expensiveCalculationAsyncCode expensiveCalculationAsync "Use async{}" "Expensive calculation (async)"
let pageWorker = pageGeneral expensiveCalculationWorkerCode expensiveCalculationWorker "Concurrency (web workers)" "Expensive calculation (web worker)"

let pageVariablePeformance (model:Model) dispatch =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [
              Heading.h1 [ ]
                [ str "Unstable performance JavaScript" ]
              Image.image
                [
                  
                  Image.Option.CustomClass "IsInlineBlock"
                ]
                [ img [ Src "images/javaScriptPerformance.png" ] ]
            ]
        ]
    ]

let page3percentWebassembly (model:Model) dispatch =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [
              Heading.h1 [ ]
                [ str "For the 3%" ]
              Image.image
                [
                  
                  Image.Option.CustomClass "IsInlineBlock"
                ]
                [ img [ Src "images/javaScriptPerformance.png" ] ]
                //https://jsperf.com/checking-for-null-or-undefined
            ]


        ]
    ]

let pageWasm (model:Model) dispatch =
  pageGeneralTwoColumn
    (fsharpEditor model dispatch doExpensiveCalculationWasmCode)
    (cppEditor model dispatch doExpensiveCalculationWasmCppCode)
    doExpensiveCalculationWasm
    "Predictable performance (wasm)" "Expensive calculation (wasm)"
    model
    dispatch

//let pageEnergyCalculation = pageGeneral energyCalculationCode energyCaclulation "Calculating on DNA" "DNA energy caclulation"
let pageEnergyCalculation (model:Model) dispatch =
  pageGeneralTwoColumn
    (fsharpEditor model dispatch energyCalculationCode)
    (cppEditor model dispatch energyCalculationCppCode) 
    energyCaclulation
    "Calculating on DNA" "DNA energy caclulation"
    model
    dispatch

let pageSummary (model:Model) dispatch  =
  Hero.hero
    [
      Hero.IsFullHeight ]
    [
      Hero.body
        [ ]
        [ Container.container [ Container.IsFluid
                                Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [
              Heading.h1
                []
                [ str "Questions?" ]

              a
                [Href "https://github.com/cgravill/browserComputeFSharp"]
                [str "https://github.com/cgravill/browserComputeFSharp"]
              p
                []
                [str ("(will be live)")]
            ]

            
        ]
    ]

//WebSharper
let [<Global>] Library: obj = jsNative

let WebSharperCalculation dispatch =
  Library?startWorker (fun (message:string) -> dispatch (UpdatedOutputs message))

let WebSharperCalculationCode = """[<WebSharper.JavaScriptExport>]
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
            worker.Terminate()"""

let pageWebSharperCalculation = pageGeneral WebSharperCalculationCode WebSharperCalculation "Mixing C# & F#" "WebSharper calculation"

let WebSharperUtilityCode = """using WebSharper;

namespace LibraryCS
{
	[JavaScript]
	public class Utility
    {
        public int Process(int value)
        {
            var random = new System.Random();
            var wait = random.Next(4);

            var finish = System.DateTime.Now.AddSeconds(wait);
            do {

            } while (System.DateTime.Now < finish);
            return value * 3;
        }
    }
}"""

let pageWebSharperFsCS (model:Model) dispatch =
  pageGeneralTwoColumn
    (fsharpEditor model dispatch WebSharperCalculationCode)
    (csharpEditor model dispatch WebSharperUtilityCode) 
    energyCaclulation
    "Predictable performance (wasm)" "Expensive calculation (wasm)"
    model
    dispatch


let WebSharperLibaryCode = """// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

IntelliFactory = {
    Runtime: {
        Ctor: function (ctor, typeFunction) {
            ctor.prototype = typeFunction.prototype;
            return ctor;
        },

        Class: function (members, base, statics) {
            var proto = members;
            if (base) {
                proto = new base();
                for (var m in members) { proto[m] = members[m] }
            }
            var typeFunction = function (copyFrom) {
                if (copyFrom) {
                    for (var f in copyFrom) { this[f] = copyFrom[f] }
                }
            }
            typeFunction.prototype = proto;
            if (statics) {
                for (var f in statics) { typeFunction[f] = statics[f] }
            }
            return typeFunction;
        },

        Clone: function (obj) {
            var res = {};
            for (var p in obj) { res[p] = obj[p] }
            return res;
        },

        NewObject:
            function (kv) {
                var o = {};
                for (var i = 0; i < kv.length; i++) {
                    o[kv[i][0]] = kv[i][1];
                }
                return o;
            },

        DeleteEmptyFields:
            function (obj, fields) {
                for (var i = 0; i < fields.length; i++) {
                    var f = fields[i];
                    if (obj[f] === void (0)) { delete obj[f]; }
                }
                return obj;
            },

        GetOptional:
            function (value) {
                return (value === void (0)) ? null : { $: 1, $0: value };
            },

        SetOptional:
            function (obj, field, value) {
                if (value) {
                    obj[field] = value.$0;
                } else {
                    delete obj[field];
                }
            },

        SetOrDelete:
            function (obj, field, value) {
                if (value === void (0)) {
                    delete obj[field];
                } else {
                    obj[field] = value;
                }
            },

        Apply: function (f, obj, args) {
            return f.apply(obj, args);
        },

        Bind: function (f, obj) {
            return function () { return f.apply(this, arguments) };
        },

        CreateFuncWithArgs: function (f) {
            return function () { return f(Array.prototype.slice.call(arguments)) };
        },

        CreateFuncWithOnlyThis: function (f) {
            return function () { return f(this) };
        },

        CreateFuncWithThis: function (f) {
            return function () { return f(this).apply(null, arguments) };
        },

        CreateFuncWithThisArgs: function (f) {
            return function () { return f(this)(Array.prototype.slice.call(arguments)) };
        },

        CreateFuncWithRest: function (length, f) {
            return function () { return f(Array.prototype.slice.call(arguments, 0, length).concat([Array.prototype.slice.call(arguments, length)])) };
        },

        CreateFuncWithArgsRest: function (length, f) {
            return function () { return f([Array.prototype.slice.call(arguments, 0, length), Array.prototype.slice.call(arguments, length)]) };
        },

        BindDelegate: function (func, obj) {
            var res = func.bind(obj);
            res.$Func = func;
            res.$Target = obj;
            return res;
        },

        CreateDelegate: function (invokes) {
            if (invokes.length == 0) return null;
            if (invokes.length == 1) return invokes[0];
            var del = function () {
                var res;
                for (var i = 0; i < invokes.length; i++) {
                    res = invokes[i].apply(null, arguments);
                }
                return res;
            };
            del.$Invokes = invokes;
            return del;
        },

        CombineDelegates: function (dels) {
            var invokes = [];
            for (var i = 0; i < dels.length; i++) {
                var del = dels[i];
                if (del) {
                    if ("$Invokes" in del)
                        invokes = invokes.concat(del.$Invokes);
                    else
                        invokes.push(del);
                }
            }
            return IntelliFactory.Runtime.CreateDelegate(invokes);
        },

        DelegateEqual: function (d1, d2) {
            if (d1 === d2) return true;
            if (d1 == null || d2 == null) return false;
            var i1 = d1.$Invokes || [d1];
            var i2 = d2.$Invokes || [d2];
            if (i1.length != i2.length) return false;
            for (var i = 0; i < i1.length; i++) {
                var e1 = i1[i];
                var e2 = i2[i];
                if (!(e1 === e2 || ("$Func" in e1 && "$Func" in e2 && e1.$Func === e2.$Func && e1.$Target == e2.$Target)))
                    return false;
            }
            return true;
        },

        ThisFunc: function (d) {
            return function () {
                var args = Array.prototype.slice.call(arguments);
                args.unshift(this);
                return d.apply(null, args);
            };
        },

        ThisFuncOut: function (f) {
            return function () {
                var args = Array.prototype.slice.call(arguments);
                return f.apply(args.shift(), args);
            };
        },

        ParamsFunc: function (length, d) {
            return function () {
                var args = Array.prototype.slice.call(arguments);
                return d.apply(null, args.slice(0, length).concat([args.slice(length)]));
            };
        },

        ParamsFuncOut: function (length, f) {
            return function () {
                var args = Array.prototype.slice.call(arguments);
                return f.apply(null, args.slice(0, length).concat(args[length]));
            };
        },

        ThisParamsFunc: function (length, d) {
            return function () {
                var args = Array.prototype.slice.call(arguments);
                args.unshift(this);
                return d.apply(null, args.slice(0, length + 1).concat([args.slice(length + 1)]));
            };
        },

        ThisParamsFuncOut: function (length, f) {
            return function () {
                var args = Array.prototype.slice.call(arguments);
                return f.apply(args.shift(), args.slice(0, length).concat(args[length]));
            };
        },

        Curried: function (f, n, args) {
            args = args || [];
            return function (a) {
                var allArgs = args.concat([a === void (0) ? null : a]);
                if (n == 1)
                    return f.apply(null, allArgs);
                if (n == 2)
                    return function (a) { return f.apply(null, allArgs.concat([a === void (0) ? null : a])); }
                return IntelliFactory.Runtime.Curried(f, n - 1, allArgs);
            }
        },

        Curried2: function (f) {
            return function (a) { return function (b) { return f(a, b); } }
        },

        Curried3: function (f) {
            return function (a) { return function (b) { return function (c) { return f(a, b, c); } } }
        },

        UnionByType: function (types, value, optional) {
            var vt = typeof value;
            for (var i = 0; i < types.length; i++) {
                var t = types[i];
                if (typeof t == "number") {
                    if (Array.isArray(value) && (t == 0 || value.length == t)) {
                        return { $: i, $0: value };
                    }
                } else {
                    if (t == vt) {
                        return { $: i, $0: value };
                    }
                }
            }
            if (!optional) {
                throw new Error("Type not expected for creating Choice value.");
            }
        },

        ScriptBasePath: "./",

        ScriptPath: function (a, f) {
            return this.ScriptBasePath + (this.ScriptSkipAssemblyDir ? "" : a + "/") + f;
        },

        OnLoad:
            function (f) {
                if (!("load" in this)) {
                    this.load = [];
                }
                this.load.push(f);
            },

        Start:
            function () {
                function run(c) {
                    for (var i = 0; i < c.length; i++) {
                        c[i]();
                    }
                }
                if ("load" in this) {
                    run(this.load);
                    this.load = [];
                }
            },
    }
}

IntelliFactory.Runtime.OnLoad(function () {
    if (self.WebSharper && WebSharper.Activator && WebSharper.Activator.Activate)
        WebSharper.Activator.Activate()
});

// Polyfill

if (!Date.now) {
    Date.now = function () {
        return new Date().getTime();
    };
}

if (!Math.trunc) {
    Math.trunc = function (x) {
        return x < 0 ? Math.ceil(x) : Math.floor(x);
    }
}

if (!Object.setPrototypeOf) {
  Object.setPrototypeOf = function (obj, proto) {
    obj.__proto__ = proto;
    return obj;
  }
}

function ignore() { };
function id(x) { return x };
function fst(x) { return x[0] };
function snd(x) { return x[1] };
function trd(x) { return x[2] };

if (!console) {
    console = {
        count: ignore,
        dir: ignore,
        error: ignore,
        group: ignore,
        groupEnd: ignore,
        info: ignore,
        log: ignore,
        profile: ignore,
        profileEnd: ignore,
        time: ignore,
        timeEnd: ignore,
        trace: ignore,
        warn: ignore
    }
}
;
(function()
{
 "use strict";
 var Global,EventTarget,WindowOrWorkerGlobalScope,WorkerGlobalScope,WebSharper,Obj,LibraryCS,Utility,Event,Random,Operators,IntelliFactory,Runtime,Date,Math;
 Global=self;
 EventTarget=Global.EventTarget;
 WindowOrWorkerGlobalScope=Global.WindowOrWorkerGlobalScope;
 WorkerGlobalScope=Global.WorkerGlobalScope;
 WebSharper=Global.WebSharper=Global.WebSharper||{};
 Obj=WebSharper.Obj=WebSharper.Obj||{};
 LibraryCS=Global.LibraryCS=Global.LibraryCS||{};
 Utility=LibraryCS.Utility=LibraryCS.Utility||{};
 Event=Global.Event;
 Random=WebSharper.Random=WebSharper.Random||{};
 Operators=WebSharper.Operators=WebSharper.Operators||{};
 IntelliFactory=Global.IntelliFactory;
 Runtime=IntelliFactory&&IntelliFactory.Runtime;
 Date=Global.Date;
 Math=Global.Math;
 Obj=WebSharper.Obj=Runtime.Class({},null,Obj);
 Obj.New=Runtime.Ctor(function()
 {
 },Obj);
 Utility=LibraryCS.Utility=Runtime.Class({
  Process:function(value)
  {
   var random,wait,finish;
   random=new Random.New();
   wait=random.Next$1(4);
   finish=Date.now()+wait*1000;
   do;while(Date.now()<finish);
   return value*3;
  }
 },Obj,Utility);
 Utility.New=Runtime.Ctor(function()
 {
 },Utility);
 Random=WebSharper.Random=Runtime.Class({
  Next$1:function(maxValue)
  {
   return maxValue<0?Operators.FailWith("'maxValue' must be greater than zero."):Math.floor(Math.random()*maxValue);
  }
 },Obj,Random);
 Random.New=Runtime.Ctor(function()
 {
  Obj.New.call(this);
 },Random);
 Operators.FailWith=function(msg)
 {
  throw new Global.Error(msg);
 };
 Global.onmessage=function(event)
 {
  var data;
  data=event.data;
  return Global.postMessage((new Utility.New()).Process(data));
 };
}());


if (typeof IntelliFactory !=='undefined') {
  IntelliFactory.Runtime.ScriptSkipAssemblyDir = true;
  IntelliFactory.Runtime.Start();
}"""

let pageWebSharperCsJs (model:Model) dispatch =
  pageGeneralTwoColumn
    (fsharpEditor model dispatch WebSharperUtilityCode)
    (javaScriptEditor model dispatch WebSharperLibaryCode)
    WebSharperCalculation
    "Predictable performance (wasm)" "Expensive calculation (wasm)"
    model
    dispatch

let pages =
  [
    page0

    pageImage "Software & wetware" "images/mainWetlab.jpg"
    pageImage "Design-build-test-learn" "images/designBuildTestLearn.png"
    pageImage "Wetware database" "images/freezer.jpg"
    pageImage "Wetware index (barcodes)" "images/wetlabBarcodes.jpg"
    pageImage "Wetware compiler (liquid handler)" "images/liquidHandler.jpg"
    pageImage "Wetware logging (microscopy)" "images/microscope.JPG"

    pageImage "Optimising inputs" "images/optimalCombination.png"
    pageImage "Twist" "images/twist.svg"
    pageImage "Choose your mutant" "images/mutants.png"
    pageImage "Design-build-test-learn" "images/designBuildTestLearn.png"
    pageApplication

    pageImage "Design-build-test-learn" "images/designBuildTestLearn2.png"

    pagePlaceHolder "Batch vs interactive compute" "large scale on the cloud, but useful to be able to compute locally"
    
    pageImage "Running code for others" "images/cardsTweet.png" //https://twitter.com/fsibot/status/506202011423895553?s=20

    pageInferenceVideo

    pageTitle
    pageCounter1
    pageCounter2
    pageCounter3
    pageOptimiseIt
    pagePlaceHolder "Distraction" "the paper"
    pageExpensiveCalculation
    pageExpensiveCalculationAsync
    pagePlaceHolder "Why's async{} slow" "good question...."
    pagePlaceHolder "Also mailboxprocessor<T>" "good question...."
    pageWorker
    pageVariablePeformance
    pageWasm
    pageEnergyCalculation
    pagePlaceHolder "F# + WASM in workers" "diagram"
    pageSummary
    pagePlaceHolder "Change of topic, let's say you've got C# too and TypeScript" "All the things"
    pageWebSharperCalculation
    pageWebSharperFsCS
    pageWebSharperCsJs
    pageSummary
  ]

let view (model:Model) dispatch =
  let page = 
    match pages |> Seq.tryItem model.page with
    | Some page -> page
    | None -> Seq.last pages
  page model dispatch
  
let inputs dispatch =
    let update (e : KeyboardEvent, pressed) =
        match e.key with
        | "w" -> Increment |> dispatch
        | "a" -> Decrement |> dispatch
        | "ArrowLeft"
        | "ArrowUp" -> PreviousPage |> dispatch
        | "ArrowRight"
        | "ArrowDown" -> NextPage |> dispatch
        | code ->
          JS.console.log(sprintf "Code: %s" code)
    Browser.Dom.document.addEventListener("keydown", fun e -> update(e :?> _, true))




// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
//|> Program.withReactBatched "elmish-app"
|> Program.withSubscription (fun _ -> [ Cmd.ofSub inputs ] |> Cmd.batch)
|> Program.withConsoleTrace
|> Program.run
