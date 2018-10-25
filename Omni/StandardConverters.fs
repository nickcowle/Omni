namespace Omni

open FSharp.Reflection

[<RequireQualifiedAccess>]
module StandardConverters =

    let makeSimple serPair : ConverterCustomisation =
        ConverterCustomisation.makeForType (fun _ -> serPair (id, id) |> Some)

    let recordConverterInner<'a> (c : Converter) =
        let t = typeof<'a>

        if FSharpType.IsRecord t then
            let converters =
                FSharpType.GetRecordFields t
                |> Array.map (fun pi -> pi.Name, Converter.tryGetConverterUntyped c pi.PropertyType)

            if converters |> Array.forall (snd >> Option.isSome) then
                let converters = converters |> Array.map (fun (name, c) -> name, c |> Option.get)

                let toSer (r : 'a) =
                    let values = FSharpValue.GetRecordFields r
                    Seq.map2 (fun (name, cp) o -> name, (cp |> ConvertPair.toSerPair |> fst) o) converters values
                    |> Map.ofSeq

                let fromSer (m : Map<string, Serialisable>) =
                    let values = converters |> Array.map (fun (name, cp) -> Map.find name m |> (cp |> ConvertPair.toSerPair |> snd))
                    FSharpValue.MakeRecord(t, values) |> unbox

                ConvertPair.Object (toSer, fromSer) |> Some

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

                let toSer (r : 'a) =
                    let values = FSharpValue.GetTupleFields r
                    Array.map2 (ConvertPair.toSerPair >> fst) converters values

                let fromSer arr =
                    let values = Array.map2 (ConvertPair.toSerPair >> snd) converters arr
                    FSharpValue.MakeTuple(values, t) |> unbox

                ConvertPair.Array (toSer, fromSer) |> Some

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
                        yield! Seq.map2 (ConvertPair.toSerPair >> fst) cs fields
                    }
                    |> Array.ofSeq

                let fromSer (arr : Serialisable array) =
                    match arr.[0] with
                    | String s ->
                        let case, cs = Map.find s converters
                        let os = Seq.map2 (ConvertPair.toSerPair >> snd) cs (arr |> Seq.tail) |> Array.ofSeq
                        FSharpValue.MakeUnion (case, os) |> unbox
                    | _ -> failwith "Could not deserialise union - first element was not a string"

                ConvertPair.Array (toSer, fromSer) |> Some
            else
                None
        else
            None

    let unionConverter (c : Converter) =
        { new Converter with
            member __.TryGetConverter<'a> () = unionConverterInner<'a> c
        }

    let arrayConverterInner<'a> (c : Converter) =

        let bindCrate (crate : 'a ArrayTeqCrate) =
            crate.Apply
                { new ArrayTeqCrateEvaluator<_,_> with
                    member __.Eval teq =
                        let mapConvertPair cp =
                            let toSerA, fromSerA = cp |> ConvertPair.toSerPair
                            let toSer = Teq.castTo teq >> Array.map toSerA
                            let fromSer = Array.map fromSerA >> Teq.castFrom teq
                            ConvertPair.Array (toSer, fromSer)
                        c.TryGetConverter () |> Option.map mapConvertPair
                }

        Shape.tryAsArray () |> Option.bind bindCrate

    let arrayConverter (c : Converter) =
        { new Converter with
            member __.TryGetConverter<'a> () = arrayConverterInner<'a> c
        }

    let seqConverter =
        { new ConverterCustomisationWithTypeParameter with
            member __.Eval<'a> () =
                fun (c : Converter) ->
                    let mapConvertPair cp =
                        let toSerArr, fromSerArr = cp |> ConvertPair.toSerPair
                        let toSer = Seq.toArray >> toSerArr
                        let fromSer = fromSerArr >> Array.toSeq
                        ConvertPair.Serialisable (toSer, fromSer)
                    c.TryGetConverter<'a array> () |> Option.map mapConvertPair
                |> ConverterCustomisation.makeForType
        }
        |> WithTypeParamter

    let mapConverterInner<'a> (c : Converter) =

        let bindCrate (crate : 'a MapTeqCrate) =
            crate.Apply
                { new MapTeqCrateEvaluator<_,_> with
                    member __.Eval teq =
                        let mapConvertPair cp =
                            let toSerSeq, fromSerSeq = cp |> ConvertPair.toSerPair
                            let toSer = Teq.castTo teq >> Map.toSeq >> toSerSeq
                            let fromSer = fromSerSeq >> Map.ofSeq >> Teq.castFrom teq
                            ConvertPair.Serialisable (toSer, fromSer)
                        c.TryGetConverter () |> Option.map mapConvertPair
                }

        Shape.tryAsMap () |> Option.bind bindCrate

    let mapConverter (c : Converter)  =
        { new Converter with
            member __.TryGetConverter<'a> () = mapConverterInner<'a> c
        }

    let customisations =
        [
            "String", makeSimple ConvertPair.String
            "Int", makeSimple ConvertPair.Int32
            "Long", makeSimple ConvertPair.Int64
            "Float", makeSimple ConvertPair.Float
            "Bool", makeSimple ConvertPair.Bool

            "String Array", makeSimple ConvertPair.StringArray
            "Int Array", makeSimple ConvertPair.Int32Array
            "Long Array", makeSimple ConvertPair.Int64Array
            "Float Array", makeSimple ConvertPair.FloatArray
            "Bool Array", makeSimple ConvertPair.BoolArray
            "Array", ConverterCustomisation.Custom arrayConverter

            "Record", ConverterCustomisation.Custom recordConverter
            "Tuple", ConverterCustomisation.Custom tupleConverter
            "Union", ConverterCustomisation.Custom unionConverter

            "Seq", seqConverter
            "Map", ConverterCustomisation.Custom mapConverter
        ]

    let make () : CachingConverter =
        Converter.make customisations
