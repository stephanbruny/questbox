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
open QuestBox.Physics

open Box2DX.Collision
open Box2DX.Dynamics
open Box2DX.Common

let messageBus = Dispatcher.Dispatcher([| "foo"; "bar"; "input" |])


type DisplayConfig = {
    Width: int;
    Height: int;
}

type AppConfig = {
    Display : DisplayConfig;
    VirtualDisplay : DisplayConfig;
}

type Sparky (world : World) =
    inherit PhysicsObject(messageBus, world)
    let mutable animation = Animation.loadAnimation "game/assets/animation/sparky.json" |> Option.map (Animation.setAnimationState "Spark")

    override this.OnUpdate dt =
        animation <- animation |> Option.map(Animation.updateAnimation dt) 

    override this.OnDraw () =
        let pos = this.GetPosition()
        animation |> Option.map(Animation.drawAnimation (pos.X, pos.Y)) |> ignore

    [<Action ("input", "keyPressed")>]
    member this.OnKey (msg : MessageContent option) = 
        match msg with
        | Some (Text str) -> 
            match str with
            | "left" -> this.MoveX -1.0f
            | "right" -> this.MoveX 1.0f
            | "up" -> this.MoveY -1.0f
            | "down" -> this.MoveY 1.0f
            | _ -> ()
        | _ -> ()
    [<Action ("input", "keyReleased")>]
    member this.OnKeyReleased (msg : MessageContent option) = 
        match msg with
        | Some (Text str) -> 
            match str with
            | "left"
            | "right" -> this.MoveX 0.0f
            | "up"
            | "down" -> this.MoveY 0.0f
            | _ -> ()
        | _ -> ()


// type SparkyBehavior (sparky : Sparky) =
//     inherit Behavior<Sparky>(messageBus, sparky)

//     [<Action ("input", "keyPressed")>]
//     member this.OnKey (msg : MessageContent option) = 
//         match msg with
//         | Some (Text str) -> 
//             match str with
//             | "left" -> this.GameObject.MoveBody(-1.0f, 0.0f)
//             | "right" -> this.GameObject.MoveBody(1.0f, 0.0f)
//             | "up" -> this.GameObject.MoveBody(0.0f, -1.0f)
//             | "down" -> this.GameObject.MoveBody(0.0f, 1.0f)
//             | _ -> ()
//         | _ -> ()


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

    // [<Action ("input", "joystickMovement")>]
    // member this.OnJoystick (msg : MessageContent option) = 
    //     match msg with
    //     | Some (Vector2D v) ->
    //         printfn "Stick: %A" v
    //         moveX <- v.X
    //         moveY <- v.Y
    //     | _ -> ()
    
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

    [<Action ("input", "gameadButtonPressed")>]
    member this.OnJoystick (msg : MessageContent option) = 
        match msg with
        | Some v ->
            printfn "WHAT?: %A" v
        | _ -> ()

    override this.OnUpdate dt =
        // self.MoveSync (moveX * speed, moveY * speed, dt)
        cam.target <- Vector2(
            self.Position.X |> float |> System.Math.Floor |> float32,
            self.Position.Y |> float |> System.Math.Floor |> float32
        )


let createMapCollisions (world : World) (map : Tiled.MapInstance) =
    let colliders = Tiled.getMapColliderRects map
    colliders |> Array.map(fun maybeRect ->
        match maybeRect with 
        | Some rect ->
            let mutable box2dBody = BodyDef()
            box2dBody.FixedRotation <- true
            box2dBody.IsSleeping <- true
            let shape = PolygonDef()
            shape.SetAsBox(rect.width, rect.height)
            // shape.Density <- 0.0f
            let body = world.CreateBody(box2dBody)
            body.SetStatic()
            body.CreateFixture(shape) |> ignore
            body.SetPosition(Vec2(rect.x, rect.y))
            body |> Some
        | None -> None
    )

type DebugDrawBox2D () = 
    inherit DebugDraw()

    override this.DrawPolygon(points : Vec2 [], count : int, color : Color) =
        let vertices = points |> Array.map(fun p -> Vector2(p.X, p.Y))
        Raylib.DrawLineStrip(vertices, count, Color.GREEN)

    override this.DrawSolidPolygon(points : Vec2 [], count : int, color : Color) =
        let vertices = points |> Array.truncate count |> Array.rev |> Array.map(fun p -> Vector2(p.X, p.Y))
        Raylib.DrawLineStrip(vertices, count, Color.RED)

    override this.DrawCircle (center, radius, color) =
        Raylib.DrawCircle(int center.X, int center.Y, radius, Color.GREEN)

    override this.DrawSolidCircle (center, radius, axis, color) =
        Raylib.DrawCircle(int center.X, int center.Y, radius, Color.RED)

    override this.DrawSegment (p1, p2, _) =
        Raylib.DrawLine(int p1.X, int p1.Y, int p2.X, int p2.Y, Color.GREEN)

    override this.DrawXForm form = ()

[<EntryPoint>]
let main argv =

    let config = File.getJson<AppConfig> "game/conf.json"
    printf "Config: %A\n" config
    Raylib.SetConfigFlags(ConfigFlag.FLAG_VSYNC_HINT)
    Raylib.InitWindow(config.Display.Width, config.Display.Height, "Questbox")

    let canvas = Raylib.LoadRenderTexture(config.VirtualDisplay.Width, config.VirtualDisplay.Height)

    let mutable objects : BaseObject [] = Array.empty

    let addItem arr obj = arr |> Array.append [| obj |]
    let addObjects = addItem objects


    messageBus.Publish "foo" "foo" None

    Raylib.SetTargetFPS 60

    let canvasSource = Rectangle(0.0f, 0.0f, float32 config.VirtualDisplay.Width, float32 -config.VirtualDisplay.Height)
    let canvasDestination = Rectangle(0.0f, 0.0f, float32 config.Display.Width, float32 config.Display.Height)

    let (updateMap, drawMap, map) = QuestBox.Tiled.getDrawTest ()

    let drawCanvas () = 
        Raylib.DrawTexturePro(canvas.texture, canvasSource, canvasDestination, Vector2(0.0f, 0.0f), 0.0f, Color.WHITE)

    let mapping = [|
        (KeyboardKey.KEY_RIGHT |> int, "right");
        (KeyboardKey.KEY_LEFT |> int, "left");
        (KeyboardKey.KEY_UP |> int, "up");
        (KeyboardKey.KEY_DOWN |> int, "down");
    |]

    let inputActor = Input.InputActor(messageBus, mapping)


    // Box2D Stuff

    let worldBounds = AABB()
    worldBounds.LowerBound.Set(0.0f)
    worldBounds.UpperBound.Set(1000.0f)

    let world = new World(worldBounds, Vec2(0.0f, 0.0f), false)
    let debugDraw = DebugDrawBox2D()
    // world.SetDebugDraw(debugDraw)
    debugDraw.Flags <- DebugDraw.DrawFlags.Shape

    let sparky = Sparky(world)
    let cam = Camera()

    objects <- objects |> Array.append [| sparky; cam |]

    sparky.SetPosition 16.0f 16.0f

    let mapColliders = createMapCollisions world map

    while (not(Raylib.WindowShouldClose())) do
        let dt = Raylib.GetFrameTime()
        // try
        //     world.Step(dt, 8, 2)
        // with 
        // | :? System.Exception as ex -> printfn "Exception in Box2D %A" ex
        objects |> Array.Parallel.iter(fun o -> o.OnUpdate (dt))
        updateMap dt
        inputActor.EnumerateKeys()
        Raylib.BeginTextureMode canvas
        Raylib.BeginMode2D(cam.Camera)
        Raylib.ClearBackground(Color.BLACK)
        drawMap ()
        for o in objects do
            o.OnDraw()
        world.Step(dt, 8, 2)
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
