namespace Omni

type MapTeqCrate<'a> = abstract member Apply : MapTeqCrateEvaluator<'a, 'ret> -> 'ret
and MapTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, Map<'k, 'v>> -> 'ret

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
