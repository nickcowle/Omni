namespace Omni

open FSharp.Reflection

[<RequireQualifiedAccess>]
module StandardConverters =

    let makeSimple (toSer : 'a -> Serialisable) (fromSer : Serialisable -> 'a) : ConverterCustomisation =
        ConverterCustomisation.makeForType (fun _ -> toSer, fromSer)

    let recordConverter (c : Converter) : Converter =
        { new Converter with
            member __.TryGetConverter<'a> () : 'a ConvertPair option =
                let t = typeof<'a>

                if FSharpType.IsRecord t then
                    let converters =
                        FSharpType.GetRecordFields t
                        |> Array.map (fun pi -> pi.Name, Converter.tryGetConverterUntyped c pi.PropertyType)

                    if converters |> Array.forall (snd >> Option.isSome) then
                        let converters = converters |> Array.map (fun (name, c) -> name, c |> Option.get)

                        let toSer (r : 'a) : Serialisable =
                            let values = FSharpValue.GetRecordFields r
                            Seq.map2 (fun (name, (toSer, _)) o -> name, toSer o) converters values
                            |> Map.ofSeq
                            |> Serialisable.Object

                        let fromSer (s : Serialisable) : 'a =
                            match s with
                            | Object m ->
                                let values = converters |> Array.map (fun (name, (_, fromSer)) -> Map.find name m |> fromSer)
                                FSharpValue.MakeRecord(t, values) |> unbox
                            | _ -> failwith "Could not deserialise record - converted form was not an object"

                        Some (toSer, fromSer)

                    else
                        None
                else
                    None
        }

    let customisations =
        [
            "String", makeSimple Serialisable.String (function Serialisable.String s -> s | _ -> failwith "")
            "Int", makeSimple Serialisable.Int32 (function Serialisable.Int32 i -> i | _ -> failwith "")
            "Long", makeSimple Serialisable.Int64 (function Serialisable.Int64 l -> l | _ -> failwith "")
            "Float", makeSimple Serialisable.Float (function Serialisable.Float f -> f | _ -> failwith "")
            "Bool", makeSimple Serialisable.Bool (function Serialisable.Bool b -> b | _ -> failwith "")

            "String Array", makeSimple Serialisable.StringArray (function Serialisable.StringArray a -> a | _ -> failwith "")
            "Int Array", makeSimple Serialisable.Int32Array (function Serialisable.Int32Array a -> a | _ -> failwith "")
            "Long Array", makeSimple Serialisable.Int64Array (function Serialisable.Int64Array a -> a | _ -> failwith "")
            "Float Array", makeSimple Serialisable.FloatArray (function Serialisable.FloatArray a -> a | _ -> failwith "")
            "Bool Array", makeSimple Serialisable.BoolArray (function Serialisable.BoolArray a -> a | _ -> failwith "")

            "Record", ConverterCustomisation.Custom recordConverter
        ]

    let make : Converter =
        Converter.make customisations
