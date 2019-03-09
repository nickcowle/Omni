namespace Omni

[<RequireQualifiedAccess>]
module ConvertPair =

    let toSerPair (cp : 'a ConvertPair) : ('a -> Serialisable) * (Serialisable -> 'a) =

        let fail (s : Serialisable) =
            failwithf "Could not convert from Serialisable - expected a wrapped value of type %s, but instead got %A"
                (typeof<'a>.ToString ()) s

        match cp with
        | ConvertPair.String       (toSer, fromSer) -> toSer >> Serialisable.String,      (function Serialisable.String      v -> v | s -> fail s) >> fromSer
        | ConvertPair.Number       (toSer, fromSer) -> toSer >> Serialisable.Number,      (function Serialisable.Number      v -> v | s -> fail s) >> fromSer
        | ConvertPair.Bool         (toSer, fromSer) -> toSer >> Serialisable.Bool,        (function Serialisable.Bool        v -> v | s -> fail s) >> fromSer
        | ConvertPair.Object       (toSer, fromSer) -> toSer >> Serialisable.Object,      (function Serialisable.Object      v -> v | s -> fail s) >> fromSer
        | ConvertPair.Array        (toSer, fromSer) -> toSer >> Serialisable.Array,       (function Serialisable.Array       v -> v | s -> fail s) >> fromSer
        | ConvertPair.Serialisable (toSer, fromSer) -> toSer, fromSer
