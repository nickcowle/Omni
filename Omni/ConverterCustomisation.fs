namespace Omni

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ConverterCustomisation =

    let makeForType<'a> (f : Converter -> 'a ConvertPair) : ConverterCustomisation =
        { new ConverterForTypeCrate with
            member __.Apply e = e.Eval f
        }
        |> ForType

    let makeCustom (f : Converter -> Converter) : ConverterCustomisation =
        Custom f

    let tryGetConverter<'a> (customisation : ConverterCustomisation) : Converter -> 'a ConvertPair option =
        match customisation with
        | ForType crate ->
            crate.Apply
                { new ConverterForTypeEvaluator<_> with
                    member __.Eval (f : Converter -> 'b ConvertPair) =
                        if typeof<'a> = typeof<'b> then
                            let f : Converter -> 'a ConvertPair = unbox f
                            f >> Some
                        else
                            fun _ -> None
                }
        | Custom f ->
            f >> fun c -> c.TryGetConverter<'a> ()
