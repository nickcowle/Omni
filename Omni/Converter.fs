namespace Omni

open FSharp.Quotations.Patterns
open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Converter =

    let rec findRelevantCustomisation<'a>
        (customisations : (string * ConverterCustomisation) list)
        (converter : Converter)
        : 'a ConvertPair option =

        match customisations with
        | [] -> None
        | (_, c)::cs ->
            match ConverterCustomisation.tryGetConverter<'a> c converter with
            | Some f -> Some f
            | None -> findRelevantCustomisation cs converter

    let make (customisations : (string * ConverterCustomisation) list) : Converter =

        let self = ref None

        let converter =
            { new Converter with
                member __.TryGetConverter<'a> () =
                    findRelevantCustomisation<'a> customisations self.Value.Value
            }

        self := Some converter
        converter

    let private tryGetConverterUntypedInner<'a> (converter : Converter) : obj ConvertPair option =
        match converter.TryGetConverter<'a> () with
        | Some (toSer, fromSer) ->
            Some (unbox >> toSer, fromSer >> box)
        | None -> None

    let tryGetConverterUntyped (converter : Converter) (t : Type) : obj ConvertPair option =
        match <@ tryGetConverterUntypedInner @> with
        | Lambda (_, Call (_, mi, _)) ->
            let gen = mi.GetGenericMethodDefinition().MakeGenericMethod [| t |]
            let o = gen.Invoke(null, [| converter|])
            o :?> _
        | _ -> failwith "tryGetConverterUntyped failed"
