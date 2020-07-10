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

type Camera () as self = 
    inherit BaseObject(messageBus)

    let mutable cam : Camera2D = Camera2D(Vector2(0.0f, 0.0f), self.Position, 0.0f, 0.5f)
    let mutable moveX = 0.0f
    let mutable moveY = 0.0f
    
    let speed = 100.0f

    member public this.Camera = cam

    [<Action ("input", "keyPressed")>]
    member this.OnKey (msg : MessageContent option) = 
        match msg with
        | Some (Text str) ->
            match str with
            | "left" -> moveX <- -1.0f
            | "right" -> moveX <- 1.0f
            | "up" -> moveY <- -1.0f
            | "down" -> moveY <- 1.0f
            | _ -> ()
            ()
        | _ -> ()
    
    [<Action ("input", "keyReleased")>]
    member this.OnKeyReleased (msg : MessageContent option) = 
        match msg with
        | Some (Text str) ->
            match str with
            | "left" -> moveX <- 0.0f
            | "right" -> moveX <- 0.0f
            | "up" -> moveY <- 0.0f
            | "down" -> moveY <- 0.0f
            | _ -> ()
            ()
        | _ -> ()

    override this.OnUpdate dt =
        self.MoveSync (moveX * speed, moveY * speed, dt)
        cam.target <- Vector2(
            self.Position.X |> float |> System.Math.Floor |> float32,
            self.Position.Y |> float |> System.Math.Floor |> float32
        )


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
    let cam = Camera()

    objects <- objects |> Array.append [| sparky; cam |]

    messageBus.Publish "foo" "foo" None

    Raylib.SetTargetFPS 60

    let canvasSource = Rectangle(0.0f, 0.0f, float32 config.VirtualDisplay.Width, float32 -config.VirtualDisplay.Height)
    let canvasDestination = Rectangle(0.0f, 0.0f, float32 config.Display.Width, float32 config.Display.Height)

    let (updateMap, drawMap) = QuestBox.Tiled.getDrawTest ()

    let drawCanvas () = 
        Raylib.DrawTexturePro(canvas.texture, canvasSource, canvasDestination, Vector2(0.0f, 0.0f), 0.0f, Color.WHITE)


    while (not(Raylib.WindowShouldClose())) do
        let dt = Raylib.GetFrameTime()
        objects |> Array.Parallel.iter(fun o -> o.OnUpdate (dt))
        updateMap dt
        if (Raylib.IsKeyDown(KeyboardKey.KEY_RIGHT)) then messageBus.Publish "input" "keyPressed" (Some(Text "right"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_LEFT)) then messageBus.Publish "input" "keyPressed" (Some(Text "left"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_UP)) then messageBus.Publish "input" "keyPressed" (Some(Text "up"))
        if (Raylib.IsKeyDown(KeyboardKey.KEY_DOWN)) then messageBus.Publish "input" "keyPressed" (Some(Text "down"))
        if (Raylib.IsKeyReleased(KeyboardKey.KEY_RIGHT)) then messageBus.Publish "input" "keyReleased" (Some(Text "right"))
        if (Raylib.IsKeyReleased(KeyboardKey.KEY_LEFT)) then messageBus.Publish "input" "keyReleased" (Some(Text "left"))
        if (Raylib.IsKeyReleased(KeyboardKey.KEY_UP)) then messageBus.Publish "input" "keyReleased" (Some(Text "up"))
        if (Raylib.IsKeyReleased(KeyboardKey.KEY_DOWN)) then messageBus.Publish "input" "keyReleased" (Some(Text "down"))
        // if (Raylib.IsKeyDown(KeyboardKey.KEY_SPACE)) then 
        //     let n = TestActor("sparky next")
        //     objects <- objects |> Array.append [|n.GameObject|]
        //     printf "Objects: %i" objects.Length
        
        Raylib.BeginTextureMode canvas
        Raylib.BeginMode2D(cam.Camera)
        Raylib.ClearBackground(Color.BLACK)
        drawMap ()
        for o in objects do
            o.OnDraw()
        Raylib.EndMode2D()
        Raylib.EndTextureMode()

        Raylib.BeginDrawing()
        Raylib.ClearBackground(Color.BLUE)

        drawCanvas()

        Raylib.DrawFPS(10, 10)

        Raylib.EndDrawing()
        
        // messageBus.Flush ()

    // QuestBox.Tiled.test()

    Raylib.UnloadRenderTexture canvas

    Raylib.CloseWindow()

    0 // return an integer exit code
