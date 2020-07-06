namespace QuestBox

open System

module Script =

    type public AnimationObject (filename) =
        let animation = QuestBox.Animation.loadAnimation filename
        member public this.Draw (x, y) = QuestBox.Animation.drawAnimation x y

    type public Api () = 
        member public this.Print (txt : string) = Console.WriteLine(txt)
        member public this.Animation (filename : string) =
            AnimationObject(filename)