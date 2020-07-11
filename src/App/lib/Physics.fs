namespace QuestBox

open Box2DX.Collision
open Box2DX.Dynamics
open Box2DX.Common

open GameObject

module Physics =

    let getBodyBox (world : World) width height =
        let mutable box2dBody = BodyDef()
        let shape = PolygonDef()
        box2dBody.FixedRotation <- true
        shape.SetAsBox(width, height)
        shape.Density <- 1.0f
        shape.Friction <- 1.0f
        let body = world.CreateBody(box2dBody)
        body.CreateFixture(shape) |> ignore
        body.SetMassFromShapes()
        body

    type PhysicsObject (dispatcher, world) =
        inherit BaseObject(dispatcher)

        let mutable velocity = Vec2(0.0f, 0.0f)
        
        let body = getBodyBox world 1.0f 1.0f

        member this.Body = body

        member val Speed = 100.0f with get, set

        member this.Velocity 
            with get () = velocity
            and set v = 
                velocity <- v
                this.Body.SetLinearVelocity (v * this.Speed)

        override this.SetPosition x y = this.Body.SetPosition(Vec2(x, y))
        member this.GetPosition () = this.Body.GetPosition()

        member this.MoveX v = 
            this.Velocity <- Vec2(v, this.Velocity.Y)
        member this.MoveY v = 
            this.Velocity <- Vec2(this.Velocity.X, v)