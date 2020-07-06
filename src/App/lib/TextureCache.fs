namespace QuestBox

open Raylib_cs

module TextureCache =

    let mutable cache : Map<string, Texture2D> = Map.empty

    let loadTextureCached path =
        match (cache |> Map.tryFind path) with
        | Some tex -> tex
        | None ->
            let tex = Raylib.LoadTexture path
            cache <- cache |> Map.add path tex
            tex

    let clearCache () = cache |> Map.iter (fun k v -> v |> Raylib.UnloadTexture)