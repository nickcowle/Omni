namespace Omni.Test

open Omni
open System.IO
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

type IntTree = Leaf of int | Branch of IntTree * IntTree

module TestStandardConverters =

    let testRoundTripSerialisable (input : 'a) (ser : Serialisable) =

        let standard = StandardConverters.make ()
        match standard.TryGetConverter<'a> () with
        | None -> Assert.True(false, sprintf "Could not get converter for type %A" typeof<'a>)
        | Some (toSer, fromSer) ->
            Assert.Equal(ser, toSer input)
            Assert.Equal<'a>(input, fromSer ser)

    let testRoundTripJson (ser : Serialisable) =

        use writer = new StringWriter ()
        Json.serialise writer ser
        let json = writer.ToString ()
        use reader = new StringReader(json)
        let deserialised = Json.deserialise reader

        Assert.Equal(ser, deserialised)

    let testRoundTripBinary (ser : Serialisable) =

        use stream = new MemoryStream ()
        Binary.serialise stream ser
        stream.Position <- 0L
        let deserialised = Binary.deserialise stream

        Assert.Equal(ser, deserialised)

    let testRoundTrip (input : 'a) (ser : Serialisable) =

        testRoundTripSerialisable input ser
        testRoundTripJson ser
        testRoundTripBinary ser

    [<Fact>]
    let ``String round trips correctly`` () =
        let v = "foo"
        let ser = Serialisable.String "foo"
        testRoundTrip v ser

    [<Fact>]
    let ``Int round trips correctly`` () =
        let v = 1234
        let ser = Serialisable.Number (Number.Long 1234L)
        testRoundTrip v ser

    [<Fact>]
    let ``String array round trips correctly`` () =
        let v = [| "foo" ; "bar" ; "baz" |]
        let ser = Serialisable.Array [| Serialisable.String "foo" ; Serialisable.String "bar" ; Serialisable.String "baz" |]
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
                "Foo", Serialisable.Number (Number.Long 1234L)
                "Bar", Serialisable.Array [| Serialisable.String "foo" ; Serialisable.String "bar" ; Serialisable.String "baz" |]
            ]
            |> Map.ofList
            |> Serialisable.Object

        testRoundTrip record m

    [<Fact>]
    let ``Tuple round trips correctly`` () =
        let v = 1234, "foo", false
        let ser = Serialisable.Array [| Serialisable.Number (Number.Long 1234L) ; Serialisable.String "foo" ; Serialisable.Bool false |]
        testRoundTrip v ser

    [<Fact>]
    let ``Union case with no members round trips correctly`` () =
        let v = Foo
        let ser = Serialisable.Array [| Serialisable.String "Foo" |]
        testRoundTrip v ser

    [<Fact>]
    let ``Union case with one member round trips correctly`` () =
        let v = Bar 1234
        let ser = Serialisable.Array [| Serialisable.String "Bar" ; Serialisable.Number (Number.Long 1234L) |]
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

    [<Fact>]
    let ``Recursively-defined type round trips correctly`` () =

        let v = Branch (Branch (Leaf 12, Leaf 34), Branch (Leaf 56, Leaf 78))

        let ser =
            Serialisable.Array
                [|
                    Serialisable.String "Branch"
                    Serialisable.Array
                        [|
                            Serialisable.String "Branch"
                            Serialisable.Array [| Serialisable.String "Leaf" ; Serialisable.Number (Number.Long 12L) |]
                            Serialisable.Array [| Serialisable.String "Leaf" ; Serialisable.Number (Number.Long 34L) |]
                        |]
                    Serialisable.Array
                        [|
                            Serialisable.String "Branch"
                            Serialisable.Array [| Serialisable.String "Leaf" ; Serialisable.Number (Number.Long 56L) |]
                            Serialisable.Array [| Serialisable.String "Leaf" ; Serialisable.Number (Number.Long 78L) |]
                        |]
                |]

        testRoundTrip v ser

    [<Fact>]
    let ``Requests for a recursively-defined type only occur once`` () =

        let standard = StandardConverters.make ()
        let requested = ResizeArray ()
        standard.ConverterRequested.Add requested.Add

        let v = Branch (Branch (Leaf 12, Leaf 34), Branch (Leaf 56, Leaf 78))

        match standard.TryGetConverter<IntTree> () with
        | Some (toSer, _) ->
            v |> toSer |> ignore
        | None ->
            Assert.True false

        let expected =
            [
                typeof<IntTree>, false
                typeof<int>, false
            ]

        Assert.Equal(expected, requested)

    [<Fact>]
    let ``String sequence round trips correctly`` () =
        let v = [ "foo" ; "bar" ; "baz" ] |> Seq.ofList
        let ser = Serialisable.Array [| Serialisable.String "foo" ; Serialisable.String "bar" ; Serialisable.String "baz" |]
        testRoundTrip v ser

    [<Fact>]
    let ``Map from strings to ints round trips correctly`` () =
        let v = [ "foo", 1234 ; "bar", 5678 ] |> Map.ofSeq
        let ser =
            Serialisable.Array
                [|
                    Serialisable.Array [| Serialisable.String "bar" ; Serialisable.Number (Number.Long 5678L) |]
                    Serialisable.Array [| Serialisable.String "foo" ; Serialisable.Number (Number.Long 1234L) |]
                |]

        testRoundTrip v ser
