namespace QuestBox

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
        FlipD : bool;
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
         ``firstgid``: uint32;
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
            FlipD = gid &&& maskFlipXY <> 0u;
        }

    type Tile = {
        SourceRect : Rectangle;
        Attributes : TiledTile option;
    }

    let textureCache = Cache.GenericCache<Texture2D>(Raylib.LoadTexture, fun path texture -> Raylib.UnloadTexture texture)

    let getTileTileset tile tilesets =
        match tilesets with
        | [| ts |] -> ts
        | _ -> 
            tilesets |> Array.filter(fun tileset -> tileset.firstgid <= tile.Gid) |> Array.sortBy (fun ts -> ts.firstgid) |> Array.last

    let findTiledTile tileset gid = tileset.tiles |> Array.tryFind (fun t -> t.id = gid)

    let getTileInstance tileset (gid : uint32) =
        let tileWidth = tileset.tilewidth + tileset.spacing
        let tileHeight = tileset.tileheight + tileset.spacing
        let rows = tileset.columns |> uint32 // tileset.imagewidth / tileWidth |> uint32
        let tileIndex = gid - 1u
        let x = tileIndex % rows |> float32
        let y = tileIndex / rows |> float32
        {
            SourceRect = Rectangle(x * (float32 tileWidth), y * (float32 tileHeight), float32 tileset.tilewidth, float32 tileset.tileheight);
            Attributes = findTiledTile tileset gid
        }

    let toRad num = (System.Math.PI / 180.0) * num;

    let renderTileLayer map tilesets (layer : TiledTileReference []) =
        layer |> Array.iteri(fun idx tile ->
            if tile.Gid > 0u then
                let tileset = getTileTileset tile tilesets
                let rows = map.width
                let x = (idx % rows) * tileset.tilewidth |> float32
                let y = (idx / rows) * tileset.tileheight |> float32
                let tileInst = getTileInstance tileset tile.Gid
                let texture = textureCache.Load ("game/assets/map/tilesets/" + tileset.image)

                let modX = (if tile.FlipX && not (tile.FlipD) then -1 else 1) |> float32
                let modY = (if tile.FlipY && not (tile.FlipD) then -1 else 1) |> float32
                let modR = if tile.FlipY then -90.0f else 90.0f
                let r = (if tile.FlipD then modR else 0.0f)
                let ox = if tile.FlipX then 0.0f else 16.0f
                let oy = if tile.FlipY then 0.0f else 16.0f
                let origin = if tile.FlipD then Vector2(ox, oy) else Vector2(0.0f, 0.0f)
                let rect = Rectangle( tileInst.SourceRect.x, tileInst.SourceRect.y, tileInst.SourceRect.width * modX, tileInst.SourceRect.height * modY )

                Raylib.DrawTexturePro(
                    texture,
                    rect,
                    Rectangle(x, y, tileset.tilewidth |> float32, tileset.tileheight |> float32),
                    origin,
                    r,
                    Color.WHITE
                )
        )

    let test () = 

        let map = File.getJson<TiledMap> "game/assets/map/dungeon.json"
        let layerData = map.layers |> Array.filter(fun layer -> layer.``type`` = "tilelayer") |> Array.map(fun layer -> layer.data)
        let layers = layerData |> Array.map(fun l -> l |> Array.map getTile)
        let tilesets = map.tilesets |> Array.map(fun ts -> 
            { File.getJson<TiledTileset> ("game/assets/map/" + ts.source) with firstgid = ts.firstgid }
        )
        printfn "Map: %A, %A" layers tilesets

    let getDrawTest () = 
        let map = File.getJson<TiledMap> "game/assets/map/dungeon.json"
        let layerData = map.layers |> Array.filter(fun layer -> layer.``type`` = "tilelayer") |> Array.map(fun layer -> layer.data)
        let layers = layerData |> Array.map(fun l -> l |> Array.map getTile)
        let tilesets = map.tilesets |> Array.map(fun ts -> 
            { File.getJson<TiledTileset> ("game/assets/map/" + ts.source) with firstgid = ts.firstgid }
        )
        fun () ->
            layers |> Array.iter(fun l -> l |> renderTileLayer map tilesets) |> ignore