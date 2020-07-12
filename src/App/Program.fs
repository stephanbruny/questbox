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
open QuestBox.Audio

open Box2DX.Collision
open Box2DX.Dynamics
open Box2DX.Common

let messageBus = Dispatcher.Dispatcher([| "system"; "game"; "player"; "input"; "objects"; "audio" |])


type DisplayConfig = {
    Width: int;
    Height: int;
}

type AppConfig = {
    Display : DisplayConfig;
    VirtualDisplay : DisplayConfig;
}

type SpriteDirection =
| Left
| Right
| Up
| Down

type Sparky (world : World) =
    inherit PhysicsObject(messageBus, world, Vec2(8.0f, 8.0f))
    let mutable animation = Animation.loadAnimation "game/assets/animation/zoi.json" |> Option.map (Animation.setAnimationState "Default")

    let mutable lastAnimationState = "Default"

    let setAnimationState name =
        if name <> lastAnimationState then
            lastAnimationState <- name
            animation <- Animation.setAnimationState name animation.Value |> Some

    override this.OnUpdate dt =
        animation <- animation |> Option.map(Animation.updateAnimation dt) 

    override this.OnDraw () =
        let pos = this.GetCenter()
        animation |> Option.map(Animation.drawAnimation (pos.X, pos.Y)) |> ignore


    override this.OnMove v =
        if v.X < 0.0f then setAnimationState "WalkLeft"
        if v.X > 0.0f then setAnimationState "WalkRight"
        if v.Y < 0.0f then setAnimationState "WalkUp"
        if v.Y > 0.0f then setAnimationState "WalkDown"

        if v.X = 0.0f && v.Y = 0.0f then
            let state =
                match lastAnimationState with
                | "WalkLeft" -> "Left"
                | "WalkRight" -> "Right"
                | "WalkUp" -> "Up"
                | "WalkDown" -> "Down"
                | _ -> lastAnimationState
            // printfn "Last State: %s" lastAnimationState
            setAnimationState state

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

type Ball (world : World) = 
    inherit PhysicsObject(messageBus, world, Vec2(8.0f, 8.0f))

    let mutable animation = Animation.loadAnimation "game/assets/animation/sparky.json" |> Option.map (Animation.setAnimationState "Spark")

    override this.OnUpdate dt =
        animation <- animation |> Option.map(Animation.updateAnimation dt) 

    override this.OnDraw () =
        let pos = this.GetCenter()
        animation |> Option.map(Animation.drawAnimation (pos.X, pos.Y)) |> ignore

    override this.OnCollide ids =
        printfn "Collision %A" ids
        this.Publish "audio" "play" (Some (Text "kick"))

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
            shape.SetAsBox(rect.width / 2.0f, rect.height / 2.0f)
            // shape.Density <- 0.0f
            let body = world.CreateBody(box2dBody)
            body.SetStatic()
            body.CreateFixture(shape) |> ignore
            body.SetPosition(Vec2(rect.x + rect.width / 2.0f, rect.y + rect.height / 2.0f))
            body |> Some
        | None -> None
    )

[<EntryPoint>]
let main argv =

    let config = File.getJson<AppConfig> "game/conf.json"
    printf "Config: %A\n" config
    // Raylib.SetConfigFlags(ConfigFlag.FLAG_VSYNC_HINT)
    Raylib.InitWindow(config.Display.Width, config.Display.Height, "Questbox")
    Raylib.InitAudioDevice()

    let canvas = Raylib.LoadRenderTexture(config.VirtualDisplay.Width, config.VirtualDisplay.Height)

    Raylib.SetTargetFPS 60

    let canvasSource = Rectangle(0.0f, 0.0f, float32 config.VirtualDisplay.Width, float32 -config.VirtualDisplay.Height)
    let canvasDestination = Rectangle(0.0f, 0.0f, float32 config.Display.Width, float32 config.Display.Height)

    let currentMap = Tiled.loadMap "game/assets/map/dungeon.json"

    // let (updateMap, drawMap, map) = QuestBox.Tiled.getDrawTest ()

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
    world.SetContactListener(PhysicsContactListener(messageBus))
    let debugDraw = DebugDrawBox2D()
    // world.SetDebugDraw(debugDraw)
    debugDraw.Flags <- DebugDraw.DrawFlags.Shape

    let sparky = Sparky(world)
    let ball = Ball(world)
    let cam = Camera()

    let gameObjects : BaseObject [] = [| sparky; ball; cam |]

    sparky.SetPosition 16.0f 16.0f
    ball.SetPosition 48.0f 48.0f

    let mapColliders = createMapCollisions world currentMap

    let audioPlayer = AudioPlayer(messageBus, "game/assets/sound")

    let rec gameLoop map (objects : BaseObject []) = 
        if Raylib.WindowShouldClose() then () else
            let dt = Raylib.GetFrameTime()
            let nextMap = Tiled.updateMap dt map
            objects |> Array.Parallel.iter(fun o -> o.OnUpdate (dt))
            inputActor.EnumerateKeys()
            Raylib.BeginTextureMode canvas
            Raylib.BeginMode2D(cam.Camera)
            Raylib.ClearBackground(Color.BLACK)
            Tiled.drawMap map
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
            gameLoop nextMap objects

    gameLoop currentMap gameObjects

    Raylib.UnloadRenderTexture canvas

    Raylib.CloseWindow()

    0 // return an integer exit code
