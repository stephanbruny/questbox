// Learn more about F# at http://fsharp.org
open Raylib_cs
open System
open Microsoft.CodeAnalysis.CSharp.Scripting
open Microsoft.CodeAnalysis.Scripting
open System.Reflection
open System.IO
open System.Numerics

open QuestBox
open QuestBox.Dispatcher
open QuestBox.GameObject

let messageBus = Dispatcher.Dispatcher([| "foo"; "bar"; "input" |])

type SparkyBehavior (sparky : BaseObject) =
    inherit Behavior<BaseObject>(messageBus, sparky)

    [<Action ("input", "keyPressed")>]
    member this.OnKey (msg : MessageContent option) = 
        match msg with
        | Some (Text str) -> 
            match str with
            | "left" -> this.GameObject.Move(-1.0f, 0.0f)
            | "right" -> this.GameObject.Move(1.0f, 0.0f)
            | "up" -> this.GameObject.Move(0.0f, -1.0f)
            | "down" -> this.GameObject.Move(0.0f, 1.0f)
            | _ -> ()
        | _ -> ()

type DisplayConfig = {
    Width: int;
    Height: int;
}

type AppConfig = {
    Display : DisplayConfig;
    VirtualDisplay : DisplayConfig;
}

type Sparky () =
    inherit BaseObject(messageBus)
    let mutable animation = Animation.loadAnimation "game/assets/animation/sparky.json" |> Option.map (Animation.setAnimationState "Spark")

    override this.OnUpdate dt =
        animation <- animation |> Option.map(Animation.updateAnimation dt) 

    override this.OnDraw () =
        animation |> Option.map(Animation.drawAnimation (this.Position.X, this.Position.Y)) |> ignore


[<EntryPoint>]
let main argv =

    let config = File.getJson<AppConfig> "game/conf.json"
    printf "Config: %A\n" config
    Raylib.InitWindow(config.Display.Width, config.Display.Height, "Questbox")

    let canvas = Raylib.LoadRenderTexture(config.VirtualDisplay.Width, config.VirtualDisplay.Height)

    let mutable objects : BaseObject [] = Array.empty

    let addItem arr obj = arr |> Array.append [| obj |]
    let addObjects = addItem objects

    let sparky = Sparky()
    sparky.AddBehavior(SparkyBehavior)

    objects <- addObjects sparky

    messageBus.Publish "foo" "foo" None

    Raylib.SetTargetFPS 60

    let canvasSource = Rectangle(0.0f, 0.0f, float32 config.VirtualDisplay.Width, float32 -config.VirtualDisplay.Height)
    let canvasDestination = Rectangle(0.0f, 0.0f, float32 config.Display.Width, float32 config.Display.Height)

    let drawCanvas () = 
        Raylib.DrawTexturePro(canvas.texture, canvasSource, canvasDestination, Vector2(0.0f, 0.0f), 0.0f, Color.WHITE)


    while (not(Raylib.WindowShouldClose())) do
        let dt = Raylib.GetFrameTime()
        objects |> Array.Parallel.iter(fun o -> o.OnUpdate (dt))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_RIGHT)) then messageBus.Publish "input" "keyPressed" (Some(Text "right"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_LEFT)) then messageBus.Publish "input" "keyPressed" (Some(Text "left"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_UP)) then messageBus.Publish "input" "keyPressed" (Some(Text "up"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_DOWN)) then messageBus.Publish "input" "keyPressed" (Some(Text "down"))
        // if (Raylib.IsKeyDown(KeyboardKey.KEY_SPACE)) then 
        //     let n = TestActor("sparky next")
        //     objects <- objects |> Array.append [|n.GameObject|]
        //     printf "Objects: %i" objects.Length
        
        Raylib.BeginTextureMode canvas
        Raylib.ClearBackground(Color.BLUE)
        for o in objects do
            o.OnDraw()
        Raylib.EndTextureMode()

        Raylib.BeginDrawing()
        Raylib.ClearBackground(Color.BLUE)

        drawCanvas()

        Raylib.DrawFPS(10, 10)

        Raylib.EndDrawing()
        
        // messageBus.Flush ()

    OpenBox.Tiled.test()

    Raylib.UnloadRenderTexture canvas

    Raylib.CloseWindow()

    0 // return an integer exit code
