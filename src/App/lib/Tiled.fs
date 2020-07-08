namespace OpenBox

open System.Numerics
open Raylib_cs
open Newtonsoft.Json
open QuestBox

module Tiled =


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
        value : obj;
    }

    type TiledLayer = {
        id: uint32;
        data : uint32 [];
        width: int;
        height: int;
        x: int;
        y: int;
        name: string;
        opacity: float32;
        visible: bool;
        properties: TiledProperty [];
        ``type``: string;
    }

    type TiledTilesetReference = {
        firstgid : uint32;
        source : string;
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
        layers: TiledLayer [];
        tilesets : TiledTilesetReference [];
    }

    type TiledTileReference = {
        Gid : uint32;
        FlipX : bool;
        FlipY: bool;
        FlipXY : bool;
    }

    type TiledTileAnimation = {
        ``duration``: int;
        ``tileid``: uint32;
    }

    type TiledTile = {
        ``id``: uint32;
        ``properties``: TiledProperty [];
        ``animation``: TiledTileAnimation [];
    }

    type TiledTileset = {
         ``image``: string;
         ``imageheight``: int;
         ``imagewidth`` : int;
         ``margin``: int;
         ``name``: string;
         ``spacing``: int;
         ``tilecount``: int;
         ``tiledversion``: string;
         ``tileheight``: int;
         ``tilewidth``: int;
         ``columns``: int;
         ``tiles``: TiledTile [];
    }

    let getTile (gid : uint32) =
        let maskFlipX  = 0x80000000u// 0b10000000000000000000000000000000u
        let maskFlipY  = 0x40000000u //0b01000000000000000000000000000000u
        let maskFlipXY = 0x20000000u // 0b00100000000000000000000000000000u
        /// let maskGid    = 0b00011111111111111111111111111111u
        {
            Gid = gid &&& ~~~( maskFlipX ||| maskFlipY ||| maskFlipXY );
            FlipX = gid &&& maskFlipX <> 0u;
            FlipY = gid &&& maskFlipY <> 0u;
            FlipXY = gid &&& maskFlipXY <> 0u;
        }

    type Tile = {
        SourceRect : Rectangle;
        Attributes : TiledTile option;
    }

    let getTileTileset tile tilesets =
        tilesets |> Array.filter(fun tileset -> tileset.firstgid <= tile.Gid) |> Array.sortBy (fun ts -> ts.firstgid) |> Array.last

    let findTiledTile tileset gid = tileset.tiles |> Array.tryFind (fun t -> t.id = gid)

    let getTileInstance tileset (gid : uint32) =
        let x = float32 (((int gid - 1) * (tileset.tilewidth + tileset.spacing)) % tileset.imagewidth)
        let y = float32 ((int gid - 1) % tileset.columns * (tileset.tileheight + tileset.spacing))
        {
            SourceRect = Rectangle(x, y, float32 tileset.tilewidth, float32 tileset.tileheight);
            Attributes = findTiledTile tileset gid
        }

    let test () = 

        let map = File.getJson<TiledMap> "game/assets/map/dungeon.json"
        let layerData = map.layers |> Array.filter(fun layer -> layer.``type`` = "tilelayer") |> Array.map(fun layer -> layer.data)
        let layers = layerData |> Array.map(fun l -> l |> Array.map getTile)
        let tilesets = map.tilesets |> Array.map(fun ts -> File.getJson<TiledTileset> ("game/assets/map/" + ts.source))
        printfn "Map: %A, %A" layers tilesets

