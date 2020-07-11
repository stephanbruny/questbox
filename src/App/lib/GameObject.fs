namespace QuestBox

open System.Numerics

module GameObject =

    type Property = 
    | PropString of string
    | PropDecimal of int
    | PropFloat of float32
    | PropUid of System.Guid
    | PropArray of Property []
    | PropTable of Map<string, Property>

    type Behavior<'T> (dispatcher, gameObject : 'T) =
        inherit Dispatcher.Actor(dispatcher)
        member public this.GameObject = gameObject

        abstract member OnUpdate : float32 -> unit
        default this.OnUpdate dt = ()


    type BaseObject (dispatcher) =
        inherit Dispatcher.Actor (dispatcher)

        let mutable properties : Map<string, Property> = Map.empty

        let mutable behaviors : Behavior<BaseObject> [] = [||]

        let mutable position : Vector2 = Vector2(0.0f, 0.0f)

        abstract member OnUpdate : float32 -> unit
        default this.OnUpdate dt = 
            behaviors |> Array.Parallel.iter(fun behavior -> behavior.OnUpdate dt)
            ()

        abstract member OnDraw : unit -> unit;
        default this.OnDraw () = ()

        abstract member OnRemove : unit -> unit;
        default this.OnRemove () = ()

        abstract member SetPosition : float32 -> float32 -> unit
        default this.SetPosition x y = position <- Vector2(x, y)

        member public this.Move (x, y) =
            position.X <- position.X + x
            position.Y <- position.Y + y

        member public this.MoveSync (x, y, dt) =
            position.X <- position.X + (x * dt)
            position.Y <- position.Y + (y * dt)

        member public this.SetProperty (name, value) = 
            properties <- properties |> Map.add name value

        member public this.GetProperty name = properties |> Map.tryFind name

        member public this.RemoveProperty name = properties <- properties |> Map.remove name

        member public this.AddBehavior (behaviorType) = 
            let behavior = behaviorType(this)
            behaviors <- behaviors |> Array.append [| behavior |]

        member public this.Position = position
    