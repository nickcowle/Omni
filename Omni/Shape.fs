namespace Omni

type MapTeqCrate<'a> = abstract member Apply : MapTeqCrateEvaluator<'a, 'ret> -> 'ret
and MapTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, Map<'k, 'v>> -> 'ret

type ArrayTeqCrate<'a> = abstract member Apply : ArrayTeqCrateEvaluator<'a, 'ret> -> 'ret
and ArrayTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b array> -> 'ret

[<RequireQualifiedAccess>]
module Shape =

    let private makeMapTeqCrate () =
        { new MapTeqCrate<_> with
            member __.Apply e = e.Eval Teq.refl
        }

    let tryAsMap<'a> () : 'a MapTeqCrate option =
        if typedefof<'a> = typedefof<Map<_,_>> then
            Reflection.invokeStatic <@ makeMapTeqCrate @> typeof<'a>.GenericTypeArguments [||]
            |> unbox
            |> Some
        else
            None

    let private makeArrayTeqCrate () =
        { new ArrayTeqCrate<_> with
            member __.Apply e = e.Eval Teq.refl
        }

    let tryAsArray<'a> () : 'a ArrayTeqCrate option =
        if typeof<'a>.IsArray then
            Reflection.invokeStatic <@ makeArrayTeqCrate @> [| typeof<'a>.GetElementType () |] [||]
            |> unbox
            |> Some
        else
            None
