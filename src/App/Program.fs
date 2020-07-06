// Learn more about F# at http://fsharp.org
open Raylib_cs
open System
open Microsoft.CodeAnalysis.CSharp.Scripting
open Microsoft.CodeAnalysis.Scripting
open System.Reflection
open System.IO

open QuestBox
open QuestBox.Dispatcher

let rec gameLoop (onDraw) =
    while (not(Raylib.WindowShouldClose())) do
        Raylib.BeginDrawing()

        Raylib.ClearBackground(Color.BLUE)

        onDraw ()

        Raylib.EndDrawing()
        gameLoop(onDraw)

let runScript path =
    let file = File.getTextFile path
    // let compile = CSharpScript.EvaluateAsync file |> Async.AwaitTask |> Async.RunSynchronously
    let script = CSharpScript.Create (file, ScriptOptions.Default, typeof<Script.Api>)
    use stream = new MemoryStream()
    let compile = script.GetCompilation().Emit(stream)
    if compile.Success then
        let asm = Assembly.Load(stream.ToArray())
        script.RunAsync(globals = Script.Api() ) |> Async.AwaitTask |> Async.RunSynchronously |> ignore
        printf "Compile: %A" (asm.GetTypes())
    else 
        printf "Huh? %A" compile.Diagnostics
    ()

let messageBus = Dispatcher.Dispatcher([| "foo"; "bar"; "input" |])

type GameObject = {
    OnUpdate : float32 -> unit;
    OnDraw : unit -> unit;
}

type TestActor (name) as self =
    inherit Dispatcher.Actor(messageBus)

    let mutable position = (0.0f, 0.0f)
    let mutable animation = Animation.loadAnimation "game/assets/animation/sparky.json" |> Option.map (Animation.setAnimationState "Spark")

    member this.Move (x, y) = 
        let x1, y1 = position
        position <- (x1 + x, y1 + y)

    member this.SetPosition (x, y) = position <- (x, y)

    [<Action ("input", "keyPressed")>]
    member this.OnKey (msg : MessageContent option) = 
        match msg with
        | Some (Text str) -> 
            match str with
            | "left" -> self.Move(-1.0f, 0.0f)
            | "right" -> self.Move(1.0f, 0.0f)
            | "up" -> self.Move(0.0f, -1.0f)
            | "down" -> self.Move(0.0f, 1.0f)
            | _ -> ()
        | _ -> ()

    member this.GameObject = 
        {
            OnUpdate = fun dt -> 
                animation <- animation |> Option.map(Animation.updateAnimation dt) 
            OnDraw = fun () -> animation |> Option.map(Animation.drawAnimation (position)) |> ignore
        }


[<EntryPoint>]
let main argv =
    Raylib.InitWindow(1280, 720, "Questbox")

    runScript "game/script/test.csx"

    let mutable objects : GameObject [] = Array.empty

    let addItem arr obj = arr |> Array.append [| obj |]
    let addObjects = addItem objects

    let sparky = TestActor("Sparky")

    objects <- addObjects sparky.GameObject

    messageBus.Publish "foo" "foo" None

    Raylib.SetTargetFPS 60

    while (not(Raylib.WindowShouldClose())) do
        let dt = Raylib.GetFrameTime()
        objects |> Array.Parallel.iter(fun o -> o.OnUpdate (dt))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_RIGHT)) then messageBus.Publish "input" "keyPressed" (Some(Text "right"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_LEFT)) then messageBus.Publish "input" "keyPressed" (Some(Text "left"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_UP)) then messageBus.Publish "input" "keyPressed" (Some(Text "up"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_DOWN)) then messageBus.Publish "input" "keyPressed" (Some(Text "down"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_SPACE)) then 
            let n = TestActor("sparky next")
            objects <- objects |> Array.append [|n.GameObject|]
            printf "Objects: %i" objects.Length
        
        Raylib.BeginDrawing()

        Raylib.ClearBackground(Color.BLUE)

        for o in objects do
            o.OnDraw()

        Raylib.DrawFPS(10, 10)

        Raylib.EndDrawing()
        
        // messageBus.Flush ()

    OpenBox.Tiled.test()

    Raylib.CloseWindow()

    0 // return an integer exit code
