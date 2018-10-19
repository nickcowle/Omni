namespace Omni

open Newtonsoft.Json
open System.IO

[<RequireQualifiedAccess>]
module Json =

    let serialise (writer : TextWriter) (ser : Serialisable) =
        let serialiser = JsonSerializer ()
        serialiser.Serialize (writer, ser)

    let deserialise (reader : TextReader) : Serialisable =
        let serialiser = JsonSerializer ()
        serialiser.Deserialize(reader, typeof<Serialisable>) |> unbox
