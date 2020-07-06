namespace QuestBox

module Cache =

    type GenericCache<'T> (onLoad : string -> 'T, onClear : string -> 'T -> unit) =
        let mutable cache : Map<string, 'T> = Map.empty;
        member public this.Load path =
            match cache |> Map.tryFind path with
            | Some v -> v
            | None ->
                let value = onLoad path
                cache <- cache |> Map.add path value
                value

        member public this.Clear () =
            cache |> Map.iter onClear
            cache <- Map.empty