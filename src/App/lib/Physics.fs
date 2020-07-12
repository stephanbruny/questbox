namespace QuestBox

open System
open Box2DX.Collision
open Box2DX.Dynamics
open Box2DX.Common
open System.Numerics
open Raylib_cs

open Dispatcher
open GameObject

module Physics =

    let getBodyBox (world : World) (size : Vec2) density friction restitution damping userdata =
        let mutable box2dBody = BodyDef()
        let shape = PolygonDef()
        box2dBody.FixedRotation <- true
        box2dBody.LinearDamping <- damping
        // box2dBody.IsBullet <- true
        shape.SetAsBox(size.X, size.Y)
        shape.Density <- density
        shape.Friction <- friction
        shape.Restitution <- restitution
        let body = world.CreateBody(box2dBody)
        let fixture = body.CreateFixture(shape)
        fixture.UserData <- userdata
        body.SetMassFromShapes()
        body

    type DebugDrawBox2D () = 
        inherit DebugDraw()

        override this.DrawPolygon(points : Vec2 [], count : int, color : Box2DX.Dynamics.Color) =
            let vertices = points |> Array.map(fun p -> Vector2(p.X, p.Y))
            Raylib.DrawLineStrip(vertices, count, Color.GREEN)

        override this.DrawSolidPolygon(points : Vec2 [], count : int, color : Box2DX.Dynamics.Color) =
            let vertices = points |> Array.truncate count |> Array.rev |> Array.map(fun p -> Vector2(p.X, p.Y))
            Raylib.DrawLineStrip(vertices, count, Color.RED)

        override this.DrawCircle (center, radius, color) =
            Raylib.DrawCircle(int center.X, int center.Y, radius, Color.GREEN)

        override this.DrawSolidCircle (center, radius, axis, color) =
            Raylib.DrawCircle(int center.X, int center.Y, radius, Color.RED)

        override this.DrawSegment (p1, p2, _) =
            Raylib.DrawLine(int p1.X, int p1.Y, int p2.X, int p2.Y, Color.GREEN)

        override this.DrawXForm form = ()

    type PhysicsContactListener (dispatcher) = 
        inherit Actor(dispatcher)
        interface ContactListener with
            member this.BeginContact contact = 
                let (dataA, dataB) = (contact.FixtureA.UserData |> Option.ofObj, contact.FixtureB.UserData |> Option.ofObj)
                match (dataA, dataB) with
                | (Some a, Some b) ->
                    let idA = a :?> System.Guid
                    let idB = b :?> System.Guid
                    let msg = Array [| Uid idA; Uid idB |]
                    this.Publish "objects" "collision" (Some msg)
                | (Some a, None) ->
                    let idA = a :?> System.Guid
                    this.Publish "objects" "collision" (Some (Uid idA))
                | _ -> ()

            member this.EndContact contact = ()
            member this.PreSolve (contact, manifold) = 
                ()

            member this.PostSolve (contact, impulse) = 
                let impactVelocity = contact.FixtureB.Body.GetLinearVelocity() - contact.FixtureA.Body.GetLinearVelocity()
                printfn "Velo: %f, %f" impactVelocity.X impactVelocity.Y
                ()

    type PhysicsObject (dispatcher, world, size : Vec2, ?body : Body) as self =
        inherit BaseObject(dispatcher)

        let mutable velocity = Vec2(0.0f, 0.0f)

        let objectBody = body |> Option.defaultValue(getBodyBox world size 1.0f 1.0f 0.1f 0.3f (self.GetID()))

        member this.Body = objectBody


        member val Speed = 100.0f with get, set

        abstract member OnMove : Vec2 -> unit
        default this.OnMove v = ()

        abstract member OnCollide : Guid [] -> unit
        default this.OnCollide ids = ()

        member this.Velocity 
            with get () = velocity
            and set v = 
                velocity <- v
                this.Body.SetLinearVelocity (v * this.Speed)
                this.OnMove velocity

        override this.SetPosition x y = this.Body.SetPosition(Vec2(x, y))
        member this.GetPosition () = this.Body.GetPosition()
        member this.GetCenter () = this.Body.GetPosition() - size 
        member this.MoveX v = 
            this.Velocity <- Vec2(v, this.Velocity.Y)
        member this.MoveY v = 
            this.Velocity <- Vec2(this.Velocity.X, v)

        [<Action ("objects", "collision")>]
        member this.OnCollision (msg : MessageContent option) =
            match msg with
            | Some (Array ids) ->
                let uids = ids |> Array.choose(fun i -> match i with | Uid id -> Some id | _ -> None)
                match uids |> Array.tryFind(fun i -> i = this.GetID()) with
                | Some _ -> this.OnCollide (uids |> Array.filter(fun id -> id <> this.GetID()))
                | None -> ()
            | Some (Uid id) -> 
                if id = this.GetID() then this.OnCollide [||]
            | _ -> ()