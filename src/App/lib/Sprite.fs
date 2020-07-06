namespace QuestBox

open Geometry

module Sprite =

    type Sprite = {
        Position : Point2D;
        Collider : Rect2D;
    }