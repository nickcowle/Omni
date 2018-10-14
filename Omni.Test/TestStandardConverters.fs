namespace Omni.Test

open Omni
open Xunit

type TestRecordType =
    {
        Foo : int
        Bar : string array
    }

type TestUnionType =
| Foo
| Bar of int
| Baz of bool * string

module TestStandardConverters =

    let standard = StandardConverters.make

    let testRoundTrip (input : 'a) =
        match standard.TryGetConverter<'a> () with
        | Some (toSer, fromSer) ->
            let ser = toSer input
            Assert.Equal<'a>(input, ser |> fromSer)
            Assert.Equal<Serialisable>(ser, ser |> fromSer |> toSer)
        | None -> Assert.True(false, sprintf "Could not get converter for type %A" typeof<'a>)

    [<Fact>]
    let ``String round trips correctly`` () =
        testRoundTrip "foo"

    [<Fact>]
    let ``Int round trips correctly`` () =
        testRoundTrip 1234

    [<Fact>]
    let ``String array round trips correctly`` () =
        testRoundTrip [| "foo" ; "bar" ; "baz" |]

    [<Fact>]
    let ``Record type round trips correctly`` () =

        let record =
            {
                Foo = 1234
                Bar = [| "foo" ; "bar" ; "baz" |]
            }

        testRoundTrip record

    [<Fact>]
    let ``Tuple round trips correctly`` () =
        let tuple = 1234, "foo", false
        testRoundTrip tuple

    [<Fact>]
    let ``Union case with no members round trips correctly`` () =
        testRoundTrip Foo

    [<Fact>]
    let ``Union case with one member round trips correctly`` () =
        testRoundTrip (Bar 1234)

    [<Fact>]
    let ``Union case with two members round trips correctly`` () =
        testRoundTrip (Baz (true, "baz"))
