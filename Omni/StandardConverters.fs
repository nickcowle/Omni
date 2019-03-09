namespace Omni

open FSharp.Reflection

[<RequireQualifiedAccess>]
module StandardConverters =

    let makeSimple serPair : ConverterCustomisation =
        ConverterCustomisation.makeForType (fun _ -> serPair)

    let makeNumber<'a> (toString : 'a -> string) (fromString : string -> 'a) : ConverterCustomisation =
        ConverterCustomisation.makeForType (fun _ -> ConvertPair.makeNumber toString fromString)

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
                    Seq.map2 (fun (name, (toS, _)) o -> name, toS o) converters values
                    |> Map.ofSeq

                let fromSer (m : Map<string, Serialisable>) =
                    let values = converters |> Array.map (fun (name, (_, fromS)) -> Map.find name m |> fromS)
                    FSharpValue.MakeRecord(t, values) |> unbox

                ConvertPair.makeObject toSer fromSer |> Some

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
                    Array.map2 fst converters values

                let fromSer arr =
                    let values = Array.map2 snd converters arr
                    FSharpValue.MakeTuple(values, t) |> unbox

                ConvertPair.makeArray toSer fromSer |> Some

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
                        yield! Seq.map2 fst cs fields
                    }
                    |> Array.ofSeq

                let fromSer (arr : Serialisable array) =
                    match arr.[0] with
                    | String s ->
                        let case, cs = Map.find s converters
                        let os = Seq.map2 snd cs (arr |> Seq.tail) |> Array.ofSeq
                        FSharpValue.MakeUnion (case, os) |> unbox
                    | _ -> failwith "Could not deserialise union - first element was not a string"

                ConvertPair.makeArray toSer fromSer |> Some
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
                        let mapConvertPair (toSerA, fromSerA) =
                            let toSer = Teq.castTo teq >> Array.map toSerA
                            let fromSer = Array.map fromSerA >> Teq.castFrom teq
                            ConvertPair.makeArray toSer fromSer
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
                    match c.TryGetConverter<'a array> () with
                    | Some (toSerArr, fromSerArr) ->
                        let toSer = Seq.toArray >> toSerArr
                        let fromSer = fromSerArr >> Array.toSeq
                        (toSer, fromSer) |> Converter.singleton
                    | None -> Converter.empty
        }
        |> ConverterCustomisation.makeWithTypeParameter

    let mapConverterInner<'a> (c : Converter) =

        let bindCrate (crate : 'a MapTeqCrate) =
            crate.Apply
                { new MapTeqCrateEvaluator<_,_> with
                    member __.Eval teq =
                        let mapConvertPair (toSerSeq, fromSerSeq) =
                            let toSer = Teq.castTo teq >> Map.toSeq >> toSerSeq
                            let fromSer = fromSerSeq >> Map.ofSeq >> Teq.castFrom teq
                            toSer, fromSer
                        c.TryGetConverter () |> Option.map mapConvertPair
                }

        Shape.tryAsMap () |> Option.bind bindCrate

    let mapConverter (c : Converter)  =
        { new Converter with
            member __.TryGetConverter<'a> () = mapConverterInner<'a> c
        }

    let customisations =
        [
            "String", makeSimple ConvertPair.string
            "Int", makeNumber<int> (fun i -> i.ToString ()) System.Int32.Parse
            "Long", makeNumber<int64> (fun l -> l.ToString ()) System.Int64.Parse
            "Float", makeNumber<float> (fun f -> f.ToString "G17") System.Double.Parse
            "Bool", makeSimple ConvertPair.bool

            "Array", arrayConverter
            "Record", recordConverter
            "Tuple", tupleConverter
            "Union", unionConverter

            "Seq", seqConverter
            "Map", mapConverter
        ]

    let make () : CachingConverter =
        Converter.make customisations
