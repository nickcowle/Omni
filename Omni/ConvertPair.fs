﻿namespace Omni

[<RequireQualifiedAccess>]
module ConvertPair =

    let toSerPair (cp : 'a ConvertPair) : ('a -> Serialisable) * (Serialisable -> 'a) =

        let fail (s : Serialisable) =
            failwithf "Could not convert from Serialisable - expected a wrapped value of type %s, but instead got %A"
                (typeof<'a>.ToString ()) s

        match cp with
        | ConvertPair.String       (toSer, fromSer) -> toSer >> Serialisable.String,      (function Serialisable.String      v -> v | s -> fail s) >> fromSer
        | ConvertPair.Int32        (toSer, fromSer) -> toSer >> Serialisable.Int32,       (function Serialisable.Int32       v -> v | s -> fail s) >> fromSer
        | ConvertPair.Int64        (toSer, fromSer) -> toSer >> Serialisable.Int64,       (function Serialisable.Int64       v -> v | s -> fail s) >> fromSer
        | ConvertPair.Float        (toSer, fromSer) -> toSer >> Serialisable.Float,       (function Serialisable.Float       v -> v | s -> fail s) >> fromSer
        | ConvertPair.Bool         (toSer, fromSer) -> toSer >> Serialisable.Bool,        (function Serialisable.Bool        v -> v | s -> fail s) >> fromSer
        | ConvertPair.Object       (toSer, fromSer) -> toSer >> Serialisable.Object,      (function Serialisable.Object      v -> v | s -> fail s) >> fromSer
        | ConvertPair.Array        (toSer, fromSer) -> toSer >> Serialisable.Array,       (function Serialisable.Array       v -> v | s -> fail s) >> fromSer
        | ConvertPair.StringArray  (toSer, fromSer) -> toSer >> Serialisable.StringArray, (function Serialisable.StringArray v -> v | s -> fail s) >> fromSer
        | ConvertPair.Int32Array   (toSer, fromSer) -> toSer >> Serialisable.Int32Array,  (function Serialisable.Int32Array  v -> v | s -> fail s) >> fromSer
        | ConvertPair.Int64Array   (toSer, fromSer) -> toSer >> Serialisable.Int64Array,  (function Serialisable.Int64Array  v -> v | s -> fail s) >> fromSer
        | ConvertPair.FloatArray   (toSer, fromSer) -> toSer >> Serialisable.FloatArray,  (function Serialisable.FloatArray  v -> v | s -> fail s) >> fromSer
        | ConvertPair.BoolArray    (toSer, fromSer) -> toSer >> Serialisable.BoolArray,   (function Serialisable.BoolArray   v -> v | s -> fail s) >> fromSer
        | ConvertPair.Serialisable (toSer, fromSer) -> toSer, fromSer