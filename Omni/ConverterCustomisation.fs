namespace Omni

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ConverterCustomisation =

    let makeForType (f : Converter -> 'a ConvertPair) : ConverterCustomisation =
        fun c ->
            { new Converter with
                member __.TryGetConverter<'b> () =
                    if typeof<'a> = typeof<'b> then
                        f c |> box |> unbox<'b ConvertPair> |> Some
                    else
                        None
            }

    let private getWithTypeParameter<'a> (withTypeParameter : ConverterCustomisationWithTypeParameter) : ConverterCustomisation =
        withTypeParameter.Eval<'a> ()

    let makeWithTypeParameter (withTypeParameter : ConverterCustomisationWithTypeParameter) : ConverterCustomisation =
        fun c ->
            { new Converter with
                member __.TryGetConverter<'a> () =
                    let t = typeof<'a>
                    if t.IsGenericType then
                        let ts = t.GetGenericArguments ()
                        let p = ts |> Array.head
                        let customisation =
                            Reflection.invokeStatic <@ getWithTypeParameter @> [| p |] [| withTypeParameter |]
                            :?> ConverterCustomisation
                        (customisation c).TryGetConverter<'a> ()
                    else
                        None
            }
