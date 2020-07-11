namespace QuestBox

open System

module Dispatcher =

    type Number =
    | Decimal of float32
    | Integer of int

    type MessageContent =
    | Generic of obj
    | Text of string
    | Num of Number
    | Uid of Guid
    | Map of Map<string, MessageContent>
    | Vector2D of Numerics.Vector2
    | Array of MessageContent []

    type Message = {
        Subject : string * string; // Subject = Channel * Subject
        Content : MessageContent option; 
    }

    type Subscriber = Guid * (Message -> unit)

    type Channel = {
        Name : string;
        mutable Messages : Message [];
        mutable Subscribers : Subscriber [];
    }

    type ProcessorOperation =
    | Flush
    | Push of string * Message
    | Publish of string * Message

    type ProtoActor () = 
        let uuid =  Guid.NewGuid()
        member public this.GetID () = uuid
        abstract member Receive : Message -> unit
        default this.Receive msg = ()

    type Dispatcher (channelNames : string []) =
        let mutable channels = channelNames |> Array.map(fun name -> { Name = name; Messages = [||]; Subscribers = [||] })

        let processor = MailboxProcessor<ProcessorOperation>.Start(fun inbox ->
            let rec messageLoop () = async {
                let! op = inbox.Receive()
                match op with
                | Push (channel, msg) ->
                    match channels |> Array.tryFind (fun ch -> ch.Name = channel) with
                    | Some ch -> ch.Messages <- ch.Messages |> Array.append [| msg |]
                    | None ->  printf "Unknown channel %s" channel // TODO: Raise error or warning
                | Publish (channel, msg) ->
                    match channels |> Array.tryFind (fun ch -> ch.Name = channel) with
                    | Some channel -> 
                        channel.Subscribers |> Array.Parallel.iter(fun (_, action) ->
                            action msg
                        )
                    | None ->  printf "Unknown channel %s" channel // TODO: Raise error or warning
                | Flush ->
                    channels |> Array.Parallel.iter(fun channel ->
                        channel.Messages |> Array.Parallel.iter(fun msg ->
                            channel.Subscribers |> Array.Parallel.iter(fun (_, action) ->
                                action msg
                            )
                        )
                        channel.Messages <- Array.empty
                    )
                return! messageLoop()  
            }
            messageLoop()
        )

        let checkSubscription (channel : Channel) (actor : ProtoActor) =
            channel.Subscribers |> Array.tryFind(fun sub -> 
                let uid, _ = sub
                uid = actor.GetID()
            )

        let getChannel name = channels |> Array.tryFind(fun ch -> ch.Name = name)

        member public this.Subscribe channel (actor : ProtoActor) fn =
            channels <- channels |> Array.Parallel.map(fun ch ->
                if ch.Name = channel then
                    { ch with Subscribers = ch.Subscribers |> Array.append [| (actor.GetID(), fn) |] }
                else ch
            )

        member public this.Unsubscribe (actor : ProtoActor) =
            channels <- channels |> Array.map (fun channel ->
                { channel with Subscribers = channel.Subscribers |> Array.filter(fun (id, _) -> id <> actor.GetID()) }
            )

        member public this.Publish channel subject content =
            processor.Post (Publish (channel, { Subject = (channel, subject); Content = content }))

        member public this.Push channel subject content =
            processor.Post (Push (channel, { Subject = (channel, subject); Content = content }))

        member public this.Flush () =
                processor.Post Flush

    type ActorAction = (string * string) * (MessageContent option -> unit)

    type Action (channel : string, subject : string) =
        inherit System.Attribute()
        member public this.Subject = subject
        member public this.Channel = channel

    let isActionAttribute (a : obj) = 
        match a with 
        | :? Action -> true 
        | _ -> false 

    type Actor (dispatcher : Dispatcher) as self =
        inherit ProtoActor ()

        let mutable actions : ActorAction [] = Array.empty

        do
            let memberInfo = self.GetType()
            let subjectActions = memberInfo.GetMembers() |> Array.filter(fun mem -> 
                mem.GetCustomAttributes(true) |> Array.tryFind (isActionAttribute) |> Option.isSome
            )
            let staticActions = subjectActions |> Array.map(fun a ->
                let action = a.GetCustomAttributes(true) |> Array.find(isActionAttribute) :?> Action
                let method = a :?> System.Reflection.MethodInfo
                self.Subscribe action.Channel
                ActorAction ((action.Channel, action.Subject), fun (msg : MessageContent option) -> 
                    try
                        method.Invoke(self, [| msg |]) |> ignore 
                    with
                    | :? System.Exception as ex -> printfn "Exception: %s" ex.Message
                )
            )
            actions <- actions |> Array.append staticActions

        override this.Receive msg =
            match actions |> Array.tryFind(fun (subject, _) -> msg.Subject = subject) with
            | Some (_, action) -> action msg.Content
            | None -> ()

        member public this.Subscribe channel =
            dispatcher.Subscribe channel self self.Receive

        member public this.Destroy () =
            dispatcher.Unsubscribe self

        member public this.Publish channel subject content = 
            dispatcher.Publish channel subject content

        member public this.AddAction subject fn =
            actions <- actions |> Array.append [| (subject, fn) |]
        