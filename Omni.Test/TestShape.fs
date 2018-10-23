namespace Omni.Test

open Omni
open Xunit

module TestShape =

    [<Fact>]
    let ``tryAsMap returns Some for a Map type`` () =
        let crateOption = Shape.tryAsMap<Map<string, int>> ()
        Assert.True (crateOption |> Option.isSome)

    [<Fact>]
    let ``tryAsMap returns None for a Seq type`` () =
        let crateOption = Shape.tryAsMap<seq<float>> ()
        Assert.True (crateOption |> Option.isNone)
