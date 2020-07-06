namespace QuestBox

open System.Numerics
open Raylib_cs

open Geometry

module Animation = 

    type Frame = {
        Rect : Rectangle;
        Duration : float32;
    }

    type FrameData = {
        X : int;
        Y : int;
        Width : int;
        Height : int;
        Duration : float32;
    }

    type AnimationData = {
        Image : string;
        States : Map<string, FrameData array>;
    }

    type Animation = {
        Rect : Rectangle;
        CurrentFrame : Frame option;
        StateFrames : Frame array option;
        Timer : float32;
        FrameDuration : float32;
        Texture : Texture2D;
        States : Map<string, Frame array>
        State : string;
        FrameIndex : int;
    }

    let textureCache = Cache.GenericCache<Texture2D>(Raylib.LoadTexture, fun path texture -> Raylib.UnloadTexture texture)
    let animationDataCache = Cache.GenericCache<AnimationData>(File.getJson<AnimationData>, fun _ _ -> ())

    let loadFrame (data : FrameData) =
        {
            Rect = Rectangle((float32 data.X), (float32 data.Y), float32 data.Width, float32 data.Height);
            Duration = data.Duration;
        }

    let loadAnimation filepath = 
        try
            let data = animationDataCache.Load filepath
            let texture = textureCache.Load data.Image
            let frames = data.States |> Map.map (fun key value -> value |> Array.map loadFrame)
            {
                Rect = Rectangle(0.0f, 0.0f, 0.0f, 0.0f)
                CurrentFrame = None;
                FrameDuration = 0.0f;
                Texture = texture;
                States = frames;
                Timer = 0.0f;
                State = "";
                FrameIndex = 0;
                StateFrames = None;
            } |> Some
        with
        | :? System.Exception -> None

    let setAnimationState name anim =
        match anim.States |> Map.tryFind name with
        | Some state ->
            { anim with 
                FrameDuration = state.[0].Duration;
                CurrentFrame = state.[0] |> Some;
                State = name;
                StateFrames = state |> Some;
            }
           | None -> anim

    let drawAnimation (x, y) anim =
        match anim.CurrentFrame with
        | Some frame ->
            Raylib.DrawTextureRec(
                anim.Texture, 
                frame.Rect,
                Vector2(x, y),
                Color.WHITE
            )
        | None -> ()

    let setFrame index anim =
        match anim.StateFrames with
        | None -> anim
        | Some stateFrames ->
            let i = if index >= stateFrames.Length then 0 else index
            { anim with CurrentFrame = stateFrames.[i] |> Some; FrameIndex = i }

    let updateAnimation dt anim =
        match anim.CurrentFrame with
        | None -> anim
        | Some frame ->
            let timer = anim.Timer + dt
            if timer >= frame.Duration then
                let index = anim.FrameIndex + 1
                { (anim |> setFrame index) with Timer = 0.0f }
            else
                { anim with Timer = timer }