namespace QuestBox

open Raylib_cs
open Dispatcher
open System.Numerics

module Input =

    type KeyboardMapping = (int * string) []

    type Gamepad = {
        Id : int;
        Name : string;
    }

    type InputActor (dispatcher, mapping) =
        inherit Actor(dispatcher)

        let getGamepad id = 
            if Raylib.IsGamepadAvailable(LanguagePrimitives.EnumOfValue id) then
                {
                    Id = id;
                    Name = Raylib.GetGamepadName(LanguagePrimitives.EnumOfValue id); 
                } |> Some
            else None

        let rec getAnyGamepad n =
            if n < 0 then None else
                let pad = getGamepad n
                match pad with
                | Some _ -> pad
                | None -> getAnyGamepad (n - 1)

        let mutable gamepad = getAnyGamepad (4)

        do printfn "Gamepad: %A" gamepad

        member public this.EnumerateKeys () = 
            mapping |> Array.Parallel.iter(fun (keycode, name) ->
                let key : KeyboardKey = LanguagePrimitives.EnumOfValue keycode
                if (Raylib.IsKeyDown(key)) then
                    this.Publish "input" "keyPressed" (Text name |> Some)
                if (Raylib.IsKeyReleased(key)) then
                    this.Publish "input" "keyReleased" (Text name |> Some)
            )

        member public this.EnumerateGamepad id = 
            if Raylib.IsGamepadAvailable(id) then
                match gamepad with
                | Some pad -> ()
                | None ->
                    gamepad <- getGamepad (id |> int)
                    printfn "Now it's there??? %A" gamepad
                let leftStick = Vector2(
                    Raylib.GetGamepadAxisMovement(id, GamepadAxis.GAMEPAD_AXIS_LEFT_X),
                    Raylib.GetGamepadAxisMovement(id, GamepadAxis.GAMEPAD_AXIS_LEFT_Y)
                )
                this.Publish "input" "joystickMovement" (Vector2D leftStick |> Some)

                let rightStick = Vector2(
                    Raylib.GetGamepadAxisMovement(id, GamepadAxis.GAMEPAD_AXIS_RIGHT_X),
                    Raylib.GetGamepadAxisMovement(id, GamepadAxis.GAMEPAD_AXIS_RIGHT_Y)
                )


                let button = Raylib.GetGamepadButtonPressed () |> Number.Integer |> Num
                this.Publish "input" "gamepadButtonPressed" (Some button)