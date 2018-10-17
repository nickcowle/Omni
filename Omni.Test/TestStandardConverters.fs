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

    let standard = StandardConverters.make ()

    let testRoundTrip (input : 'a) (ser : Serialisable) =
        match standard.TryGetConverter<'a> () with
        | Some (toSer, fromSer) ->
            Assert.Equal(ser, toSer input)
            Assert.Equal<'a>(input, fromSer ser)
        | None -> Assert.True(false, sprintf "Could not get converter for type %A" typeof<'a>)

    [<Fact>]
    let ``String round trips correctly`` () =
        let v = "foo"
        let ser = Serialisable.String "foo"
        testRoundTrip v ser

    [<Fact>]
    let ``Int round trips correctly`` () =
        let v = 1234
        let ser = Serialisable.Int32 1234
        testRoundTrip v ser

    [<Fact>]
    let ``String array round trips correctly`` () =
        let v = [| "foo" ; "bar" ; "baz" |]
        let ser = Serialisable.StringArray [| "foo" ; "bar" ; "baz" |]
        testRoundTrip v ser

    [<Fact>]
    let ``Record type round trips correctly`` () =

        let record =
            {
                Foo = 1234
                Bar = [| "foo" ; "bar" ; "baz" |]
            }

        let m =
            [
                "Foo", Serialisable.Int32 1234
                "Bar", Serialisable.StringArray [| "foo" ; "bar" ; "baz" |]
            ]
            |> Map.ofList
            |> Serialisable.Object

        testRoundTrip record m

    [<Fact>]
    let ``Tuple round trips correctly`` () =
        let v = 1234, "foo", false
        let ser = Serialisable.Array [| Serialisable.Int32 1234 ; Serialisable.String "foo" ; Serialisable.Bool false |]
        testRoundTrip v ser

    [<Fact>]
    let ``Union case with no members round trips correctly`` () =
        let v = Foo
        let ser = Serialisable.Array [| Serialisable.String "Foo" |]
        testRoundTrip v ser

    [<Fact>]
    let ``Union case with one member round trips correctly`` () =
        let v = Bar 1234
        let ser = Serialisable.Array [| Serialisable.String "Bar" ; Serialisable.Int32 1234 |]
        testRoundTrip v ser

    [<Fact>]
    let ``Union case with two members round trips correctly`` () =
        let v = Baz (true, "baz")
        let ser = Serialisable.Array [| Serialisable.String "Baz" ; Serialisable.Bool true ; Serialisable.String "baz" |]
        testRoundTrip v ser

    [<Fact>]
    let ``Repeated requests for an int Converter are cached`` () =

        let standard = StandardConverters.make ()
        let requested = ResizeArray ()
        standard.ConverterRequested.Add requested.Add

        standard.TryGetConverter<int> () |> ignore
        standard.TryGetConverter<int> () |> ignore
        standard.TryGetConverter<int> () |> ignore

        let expected =
            [
                typeof<int>, false
                typeof<int>, true
                typeof<int>, true
            ]

        Assert.Equal(expected, requested)
