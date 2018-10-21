namespace Omni

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

    let private makeCached (c : Converter) : CachingConverter =

        let converters = System.Collections.Generic.Dictionary ()
        let e = Event<_> ()

        { new CachingConverter with
            member __.TryGetConverter<'a> () =

                let t = typeof<'a>
                let name = t.FullName
                match converters.TryGetValue name with
                | true, c ->
                    e.Trigger (t, true)
                    c |> unbox |> Some
                | false, _ ->
                    e.Trigger (t, false)
                    match c.TryGetConverter<'a> () with
                    | Some c ->
                        converters.Add(name, c |> box)
                        Some c
                    | None -> None

            member __.ConverterRequested = e.Publish
        }

    let private withConvertPair (pair : 'a ConvertPair) (c : Converter) =

        { new Converter with
            member __.TryGetConverter<'b> () =
                if typeof<'b> = typeof<'a> then
                    pair |> unbox |> Some
                else
                    c.TryGetConverter<'b> ()
        }

    let make (customisations : (string * ConverterCustomisation) list) : CachingConverter =

        let self : CachingConverter option Ref = ref None

        let converter =
            { new Converter with
                member __.TryGetConverter<'a> () =

                    let pair = ref None

                    let withAConverter =
                        let toSer a = fst pair.Value.Value a
                        let fromSer s = snd pair.Value.Value s
                        withConvertPair (toSer, fromSer) self.Value.Value

                    findRelevantCustomisation<'a> customisations withAConverter
                    |> Option.map (fun p -> pair := Some p ; p)
            }

        let cached = makeCached converter

        self := Some cached
        cached

    let private tryGetConverterUntypedInner<'a> (converter : Converter) : obj ConvertPair option =
        match converter.TryGetConverter<'a> () with
        | Some (toSer, fromSer) ->
            Some (unbox >> toSer, fromSer >> box)
        | None -> None

    let tryGetConverterUntyped (converter : Converter) (t : Type) : obj ConvertPair option =
        Reflection.invokeStatic <@ tryGetConverterUntypedInner @> [| t |] [| converter|] |> unbox
