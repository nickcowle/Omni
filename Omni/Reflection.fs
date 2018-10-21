namespace Omni

open FSharp.Quotations
open FSharp.Quotations.Patterns
open System

[<RequireQualifiedAccess>]
module Reflection =

    let invokeStatic (quot : 'a Expr) (types : Type array) (parameters : obj array) : obj =

        match quot with
        | Lambda (_, Call (_, mi, _)) ->
            let gen = mi.GetGenericMethodDefinition().MakeGenericMethod types
            gen.Invoke(null, parameters)
        | _ -> failwith "invokeStatic failed"
