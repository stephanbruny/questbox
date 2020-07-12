namespace QuestBox

open System
open System.IO
open Raylib_cs

open Dispatcher

module Audio =

    type AudioPlayer (dispatcher, soundsPath) = 
        inherit Actor(dispatcher)

        let sounds =
            let files = Directory.GetFiles (soundsPath, "*.ogg")
            files |> Array.map(fun f -> (Path.GetFileNameWithoutExtension f, Raylib.LoadSound f))

        [<Action ("audio", "play")>]
        member this.OnPlay (msg : MessageContent option) = 
            match msg with
            | Some (Text str) -> 
                match sounds |> Array.tryFind(fun (name, sound) -> name = str) with
                | Some (_, sound) ->
                    Raylib.PlaySound sound
                | None -> ()
            | _ -> ()





