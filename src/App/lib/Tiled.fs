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
        ``animation``: TiledTileAnimation [];
        ``properties``: TiledProperty [];
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

    type TileAnimation = {
        Time : float32;
        CurrentGid : uint32;
        CurrentDuration : float32;
    }

    type Tile = {
        SourceRect : Rectangle;
        Attributes : TiledTile option;
        Time : float32;
        AnimationIndex : int;
        Tileset : TiledTileset;
        Reference : TiledTileReference;
    }

    type Layer = Tile option []

    type MapInstance = {
        Map : TiledMap;
        Layers : Layer [];
    }

    let textureCache = Cache.GenericCache<Texture2D>(Raylib.LoadTexture, fun path texture -> Raylib.UnloadTexture texture)

    let getTileTileset tile tilesets =
        match tilesets with
        | [| ts |] -> ts
        | _ -> 
            tilesets |> Array.filter(fun tileset -> tileset.firstgid <= tile.Gid) |> Array.sortBy (fun ts -> ts.firstgid) |> Array.last

    let findTiledTile tileset gid = tileset.tiles |> Array.tryFind (fun t -> t.id = gid - tileset.firstgid)

    let getTileSourceRect tileset gid =
        let tileWidth = tileset.tilewidth + tileset.spacing
        let tileHeight = tileset.tileheight + tileset.spacing
        let rows = tileset.columns |> uint32 // tileset.imagewidth / tileWidth |> uint32
        let tileIndex = gid - 1u
        let x = tileIndex % rows |> float32
        let y = tileIndex / rows |> float32
        Rectangle(x * (float32 tileWidth), y * (float32 tileHeight), float32 tileset.tilewidth, float32 tileset.tileheight)


    let getTileInstance tileset (tile : TiledTileReference) =
        {
            SourceRect = getTileSourceRect tileset tile.Gid
            Attributes = findTiledTile tileset tile.Gid;
            Time = 0.0f;
            AnimationIndex = 0;
            Tileset = tileset;
            Reference = tile;
        }

    let updateTile dt (tile : Tile) =
        match tile.Attributes with
        | Some attributes ->
            if not (isNull attributes.animation) then
                let animationArray = attributes.animation
                let time = tile.Time + dt
                let current = animationArray.[tile.AnimationIndex]
                let duration = (float32 current.duration) / 1000.0f
                if time >= duration then 
                    let i = tile.AnimationIndex + 1
                    let newIndex = if i >= animationArray.Length then 0 else i
                    let sourceRect = getTileSourceRect tile.Tileset (animationArray.[newIndex].tileid + 1u)
                    { tile with Time = 0.0f; AnimationIndex = newIndex; SourceRect = sourceRect}
                else
                    { tile with Time = tile.Time + dt }
            else tile
        | None -> tile
        

    let updateTiles dt (tiles : Tile option []) =
        tiles |> Array.Parallel.map (Option.map(updateTile dt))

    let toRad num = (System.Math.PI / 180.0) * num;

    let getInstanceLayer tilesets (layer : TiledTileReference []) =
        layer |> Array.map(fun tile ->
            if tile.Gid > 0u then
                let tileset = getTileTileset tile tilesets
                getTileInstance tileset tile |> Some
            else None
        )

    let getTilePosition map index =
        Vector2(
            (index % map.width) * map.tilewidth |> float32,
            (index / map.width) * map.tileheight |> float32
        )

    let getTileRect map index =
        let pos = getTilePosition map index
        Rectangle(pos.X, pos.Y, map.tilewidth |> float32, map.tileheight |> float32)

    let renderTileLayer map (layer : Tile option []) =
        layer |> Array.iteri(fun idx maybeTile ->
            match maybeTile with 
            | Some tile ->
                let rows = map.width
                let x = (idx % rows) * tile.Tileset.tilewidth |> float32
                let y = (idx / rows) * tile.Tileset.tileheight |> float32
                let texture = textureCache.Load ("game/assets/map/tilesets/" + tile.Tileset.image)

                let modX = (if tile.Reference.FlipX && not (tile.Reference.FlipD) then -1 else 1) |> float32
                let modY = (if tile.Reference.FlipY && not (tile.Reference.FlipD) then -1 else 1) |> float32
                let modR = if tile.Reference.FlipY then -90.0f else 90.0f
                let r = (if tile.Reference.FlipD then modR else 0.0f)
                let ox = if tile.Reference.FlipX then 0.0f else 16.0f
                let oy = if tile.Reference.FlipY then 0.0f else 16.0f
                let origin = if tile.Reference.FlipD then Vector2(ox, oy) else Vector2(0.0f, 0.0f)
                let rect = Rectangle( tile.SourceRect.x, tile.SourceRect.y, tile.SourceRect.width * modX, tile.SourceRect.height * modY )

                Raylib.DrawTexturePro(
                    texture,
                    rect,
                    Rectangle(x, y, tile.Tileset.tilewidth |> float32, tile.Tileset.tileheight |> float32),
                    origin,
                    r,
                    Color.WHITE
                )
            | None -> ()
        )

    let updateMap dt map =
        { map with Layers = map.Layers |> Array.Parallel.map( fun layer -> layer |> updateTiles dt ) }

    let isCollisionLayer (layer : TiledLayer) =
        if isNull layer.properties then
            false
        else
            match layer.properties |> Array.tryFind(fun prop -> prop.name = "collision" ) with
            | Some prop -> match prop.value with | :? bool  -> prop.value :?> bool | _ -> false
            | None -> false

    let getMapCollider (map : MapInstance) =
        let size = map.Map.width * map.Map.height
        let collisionLayers = map.Map.layers |> Array.filter isCollisionLayer
        let result = Array.create<bool> size false
        result |> Array.Parallel.mapi(fun idx _ ->
            match collisionLayers |> Array.tryFind(fun l -> l.data.[idx] <> 0u) with
            | Some _ -> true
            | None -> false
        )

    let getMapColliderRects (map : MapInstance) =
        let collider = getMapCollider map
        collider |> Array.Parallel.mapi(fun idx isSolid ->
            if not isSolid then 
                None
            else
                let pos = getTilePosition map.Map idx
                getTileRect map.Map idx |> Some
        )

    let getDrawTest () = 
        let map = File.getJson<TiledMap> "game/assets/map/dungeon.json"
        let layerData = map.layers |> Array.filter(fun layer -> layer.``type`` = "tilelayer") |> Array.map(fun layer -> layer.data)
        let layers = layerData |> Array.map(fun l -> l |> Array.map getTile)
        let tilesets = map.tilesets |> Array.map(fun ts -> 
            { File.getJson<TiledTileset> ("game/assets/map/" + ts.source) with firstgid = ts.firstgid }
        )
        let mutable instanceLayers = layers |> Array.map (getInstanceLayer tilesets)

        let update dt =
            instanceLayers <- instanceLayers |> Array.Parallel.map(fun l -> l |> updateTiles dt)
        let draw () = instanceLayers |> Array.iter(fun l -> l |> renderTileLayer map) |> ignore
        let mapInst = { Map = map; Layers = instanceLayers }
        (update, draw, mapInst)