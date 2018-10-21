namespace Omni

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ConverterCustomisation =

    let makeForType<'a> (f : Converter -> 'a ConvertPair option) : ConverterCustomisation =
        { new ConverterForTypeCrate with
            member __.Apply e = e.Eval f
        }
        |> ForType

    let makeCustom (f : Converter -> Converter) : ConverterCustomisation =
        Custom f

    let makeWithTypeParameter (withTypeParameter : ConverterCustomisationWithTypeParameter) : ConverterCustomisation =
        WithTypeParamter withTypeParameter

    let getWithTypeParameter<'a> (withTypeParameter : ConverterCustomisationWithTypeParameter) : ConverterCustomisation =
        withTypeParameter.Eval<'a> ()

    let rec tryGetConverter<'a> (customisation : ConverterCustomisation) : Converter -> 'a ConvertPair option =
        match customisation with
        | ForType crate ->
            crate.Apply
                { new ConverterForTypeEvaluator<_> with
                    member __.Eval (f : Converter -> 'b ConvertPair option) =
                        if typeof<'a> = typeof<'b> then
                            f |> box |> unbox<Converter -> 'a ConvertPair option>
                        else
                            fun _ -> None
                }
        | Custom f ->
            f >> fun c -> c.TryGetConverter<'a> ()
        | WithTypeParamter withTypeParameter ->
            fun c ->
                let t = typeof<'a>
                if t.IsGenericType then
                    let ts = t.GetGenericArguments ()
                    let p = ts |> Array.head
                    let customisation =
                        Reflection.invokeStatic <@ getWithTypeParameter @> [| p |] [| withTypeParameter |]
                        :?> ConverterCustomisation
                    tryGetConverter<'a> customisation c
                else
                    None
