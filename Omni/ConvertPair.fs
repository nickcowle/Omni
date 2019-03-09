namespace Omni

[<RequireQualifiedAccess>]
module ConvertPair =

    let private fail<'a> (s : Serialisable) : 'a =
        failwithf "Could not convert from Serialisable - expected a wrapped value of type %s, but instead got %A"
            (typeof<'a>.ToString ()) s

    let string = Serialisable.String, function Serialisable.String v -> v | s -> fail s
    let number = Serialisable.Number, function Serialisable.Number v -> v | s -> fail s
    let bool   = Serialisable.Bool,   function Serialisable.Bool   v -> v | s -> fail s
    let object = Serialisable.Object, function Serialisable.Object v -> v | s -> fail s
    let array  = Serialisable.Array,  function Serialisable.Array  v -> v | s -> fail s

    let map<'a, 'b>
        (toS : 'a -> Serialisable, fromS : Serialisable -> 'a)
        (toSer : 'b -> 'a) (fromSer : 'a -> 'b) =
        toSer >> toS, fromS >> fromSer

    let makeString toS fromS = map string toS fromS
    let makeNumber toS fromS = map number toS fromS
    let makeBool   toS fromS = map bool   toS fromS
    let makeObject toS fromS = map object toS fromS
    let makeArray  toS fromS = map array  toS fromS
