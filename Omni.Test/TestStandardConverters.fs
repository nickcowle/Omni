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

    type Test = abstract member Run : 'a -> Serialisable -> string -> unit

    let getConverter<'a> () : 'a ConvertPair =
        let standard = StandardConverters.make ()
        match standard.TryGetConverter<'a> () with
        | None -> failwithf "Could not get converter for type %A" typeof<'a>
        | Some cp -> cp

    let testConvertToSerialisable (value : 'a) (expected : Serialisable) =
        let (toSer, _) = getConverter ()
        Assert.Equal(expected, toSer value)

    let testConvertFromSerialisable (expected : 'a) (serialisable : Serialisable) =
        let (_, fromSer) = getConverter<'a> ()
        Assert.Equal<'a>(expected, fromSer serialisable)

    let testSerialiseToJson (serialisable : Serialisable) (expected : string) =
        use writer = new StringWriter()
        Json.serialise writer serialisable
        Assert.Equal(expected, writer.ToString ())

    let testDeserialiseFromJson (expected : Serialisable) (json : string) =
        use reader = new StringReader(json)
        let serialisable = Json.deserialise reader
        Assert.Equal(expected, serialisable)

    let testRoundTripBinarySerialisation (serialisable : Serialisable) =
        use stream = new MemoryStream ()
        Binary.serialise stream serialisable
        stream.Position <- 0L
        let deserialised = Binary.deserialise stream
        Assert.Equal(serialisable, deserialised)

    let tests =
        [
            "Convert to Serialisable",         { new Test with member __.Run v s _ = testConvertToSerialisable v s }
            "Convert from Serialisable",       { new Test with member __.Run v s _ = testConvertFromSerialisable v s }
            "Serialise to JSON",               { new Test with member __.Run _ s j = testSerialiseToJson s j }
            "Deserialise from JSON",           { new Test with member __.Run _ s j = testDeserialiseFromJson s j }
            "Round-trip binary serialisation", { new Test with member __.Run _ s _ = testRoundTripBinarySerialisation s }
        ]
        |> Map.ofList

    let runTest (testType : string) (value : 'a) (serialisable : Serialisable) (json : string) =
        let test = tests |> Map.find testType
        test.Run value serialisable json

    let testTypes =
        tests |> Map.toList |> List.map (fun (tt,_) -> [| box tt |])

    [<Theory>]
    [<MemberData"testTypes">]
    let ``String round trips correctly`` (testType : string) =
        let v = "foo"
        let ser = Serialisable.String "foo"
        let json = "\"foo\""
        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Int round trips correctly`` (testType : string) =
        let v = 1234
        let ser = Serialisable.Number (Number.Long 1234L)
        let json = "1234"
        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``String array round trips correctly`` (testType : string) =
        let v = [| "foo" ; "bar" ; "baz" |]
        let ser = Serialisable.Array [| Serialisable.String "foo" ; Serialisable.String "bar" ; Serialisable.String "baz" |]
        let json = """[
  "foo",
  "bar",
  "baz"
]"""
        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Record type round trips correctly`` (testType : string) =

        let v =
            {
                Foo = 1234
                Bar = [| "foo" ; "bar" ; "baz" |]
            }

        let ser =
            [
                "Foo", Serialisable.Number (Number.Long 1234L)
                "Bar", Serialisable.Array [| Serialisable.String "foo" ; Serialisable.String "bar" ; Serialisable.String "baz" |]
            ]
            |> Serialisable.Object

        let json = """{
  "Foo": 1234,
  "Bar": [
    "foo",
    "bar",
    "baz"
  ]
}"""

        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Tuple round trips correctly`` (testType : string) =
        let v = 1234, "foo", false
        let ser = Serialisable.Array [| Serialisable.Number (Number.Long 1234L) ; Serialisable.String "foo" ; Serialisable.Bool false |]
        let json = """[
  1234,
  "foo",
  false
]"""
        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Union case with no members round trips correctly`` (testType : string) =
        let v = Foo
        let ser = Serialisable.Array [| Serialisable.String "Foo" |]
        let json = """[
  "Foo"
]"""
        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Union case with one member round trips correctly`` (testType : string) =
        let v = Bar 1234
        let ser = Serialisable.Array [| Serialisable.String "Bar" ; Serialisable.Number (Number.Long 1234L) |]
        let json = """[
  "Bar",
  1234
]"""
        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Union case with two members round trips correctly`` (testType : string) =
        let v = Baz (true, "baz")
        let ser = Serialisable.Array [| Serialisable.String "Baz" ; Serialisable.Bool true ; Serialisable.String "baz" |]
        let json = """[
  "Baz",
  true,
  "baz"
]"""
        runTest testType v ser json

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

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Recursively-defined type round trips correctly`` (testType : string) =

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

        let json = """[
  "Branch",
  [
    "Branch",
    [
      "Leaf",
      12
    ],
    [
      "Leaf",
      34
    ]
  ],
  [
    "Branch",
    [
      "Leaf",
      56
    ],
    [
      "Leaf",
      78
    ]
  ]
]"""

        runTest testType v ser json

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

    [<Theory>]
    [<MemberData"testTypes">]
    let ``String sequence round trips correctly`` (testType : string) =
        let v = [ "foo" ; "bar" ; "baz" ] |> Seq.ofList
        let ser = Serialisable.Array [| Serialisable.String "foo" ; Serialisable.String "bar" ; Serialisable.String "baz" |]
        let json = """[
  "foo",
  "bar",
  "baz"
]"""
        runTest testType v ser json

    [<Theory>]
    [<MemberData"testTypes">]
    let ``Map from strings to ints round trips correctly`` (testType : string) =
        let v = [ "foo", 1234 ; "bar", 5678 ] |> Map.ofSeq
        let ser =
            Serialisable.Array
                [|
                    Serialisable.Array [| Serialisable.String "bar" ; Serialisable.Number (Number.Long 5678L) |]
                    Serialisable.Array [| Serialisable.String "foo" ; Serialisable.Number (Number.Long 1234L) |]
                |]
        let json = """[
  [
    "bar",
    5678
  ],
  [
    "foo",
    1234
  ]
]"""

        runTest testType v ser json
