namespace QuestBox

open System
open System.IO
open Newtonsoft.Json

module File =

    let getTextFile (path : string) =
        use reader = new StreamReader(path)
        reader.ReadToEnd ()

    let getJson<'T> (path : string) =
        getTextFile path |> JsonConvert.DeserializeObject<'T>
