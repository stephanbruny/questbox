namespace OpenBox

open FSharp.Data

module Tiled =

    type MapFile = XmlProvider<"lib/Tiled/map-format.tmx">

    type TiledPropertyValue = 
    | String of string
    | Color of string
    | Int of int
    | Float of float32
    | ObjectID of int
    | Bool of bool

    type TiledProperty = {
        name : string;
        ``type`` : string;
        value : TiledPropertyValue
    }

    type TiledMap = {
        width : int;
        height : int;
        tilewidth : int;
        tileheight : int;
        nextobjectid : int;
        renderorder : string;
        nextlayerid : int;
        tiledversion : string;
        orientation : string;
        properties : TiledProperty [];
    }

    let test () = 
        let map = MapFile.Load "game/assets/map/map-format.tmx"
        printf "Map: %A\n" map

