namespace Omni

open FSharp.Reflection

[<RequireQualifiedAccess>]
module StandardConverters =

    let makeSimple (toSer : 'a -> Serialisable) (fromSer : Serialisable -> 'a) : ConverterCustomisation =
        ConverterCustomisation.makeForType (fun _ -> Some (toSer, fromSer))

    let recordConverterInner<'a> (c : Converter) =
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

    let recordConverter (c : Converter) =
        { new Converter with
            member __.TryGetConverter<'a> () = recordConverterInner<'a> c
        }

    let tupleConverterInner<'a> (c : Converter) =
        let t = typeof<'a>

        if FSharpType.IsTuple t then
            let converters = FSharpType.GetTupleElements t |> Array.map (Converter.tryGetConverterUntyped c)

            if converters |> Array.forall Option.isSome then
                let converters = converters |> Array.map Option.get

                let toSer (r : 'a) : Serialisable =
                    let values = FSharpValue.GetTupleFields r
                    Array.map2 fst converters values
                    |> Serialisable.Array

                let fromSer (s : Serialisable) : 'a =
                    match s with
                    | Array arr ->
                        let values = Array.map2 snd converters arr
                        FSharpValue.MakeTuple(values, t) |> unbox
                    | _ -> failwith "Could not deserialise tuple - converted form was not an array"

                Some (toSer, fromSer)

            else
                None
        else
            None

    let tupleConverter (c : Converter)  =
        { new Converter with
            member __.TryGetConverter<'a> () = tupleConverterInner<'a> c
        }

    let unionConverterInner<'a> (c : Converter) =
        let t = typeof<'a>

        let tryCreateConverterForUnionCase (case : UnionCaseInfo) : obj ConvertPair array option =
            let cs = case.GetFields () |> Array.map (fun pi -> Converter.tryGetConverterUntyped c pi.PropertyType)
            if cs |> Array.forall Option.isSome then
                cs |> Array.map Option.get |> Some
            else
                None

        if FSharpType.IsUnion t then
            let cases = FSharpType.GetUnionCases t
            let converters = cases |> Array.map tryCreateConverterForUnionCase
            if converters |> Array.forall Option.isSome then
                let converters = Seq.map2 (fun (case : UnionCaseInfo) converter -> case.Name, (case, converter |> Option.get)) cases converters |> Map.ofSeq

                let toSer (u : 'a) =
                    let (case, fields) = FSharpValue.GetUnionFields(u, t)
                    let cs = Map.find case.Name converters |> snd
                    seq {
                        yield Serialisable.String case.Name
                        yield! Seq.map2 (fun (toSer, _) f -> toSer f) cs fields
                    }
                    |> Array.ofSeq
                    |> Serialisable.Array

                let fromSer (s : Serialisable) : 'a =
                    match s with
                    | Array arr ->
                        match arr.[0] with
                        | String s ->
                            let case, cs = Map.find s converters
                            let os = Seq.map2 (fun (_, fromSer) s -> fromSer s) cs (arr |> Seq.tail) |> Array.ofSeq
                            FSharpValue.MakeUnion (case, os) |> unbox
                        | _ -> failwith "Could not deserialise union - first element was not a string"
                    | _ -> failwith "Could not deserialise union - converted form was not an array"

                Some (toSer, fromSer)
            else
                None
        else
            None

    let unionConverter (c : Converter) =
        { new Converter with
            member __.TryGetConverter<'a> () = unionConverterInner<'a> c
        }

    let seqConverter =
        { new ConverterCustomisationWithTypeParameter with
            member __.Eval<'a> () =
                fun (c : Converter) ->
                    match c.TryGetConverter<'a> () with
                    | Some (toSerA, fromSerA) ->
                        let toSer = Seq.map toSerA >> Array.ofSeq >> Serialisable.Array
                        let fromSer =
                            function
                            | Array arr -> arr |> Seq.map fromSerA
                            | _ -> failwith ""
                        Some (toSer, fromSer)
                    | None -> None
                |> ConverterCustomisation.makeForType
        }
        |> WithTypeParamter

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
            "Tuple", ConverterCustomisation.Custom tupleConverter
            "Union", ConverterCustomisation.Custom unionConverter

            "Seq", seqConverter
        ]

    let make () : CachingConverter =
        Converter.make customisations
