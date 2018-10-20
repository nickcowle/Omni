namespace Omni

open System.IO
open System.Runtime.Serialization.Formatters.Binary

[<RequireQualifiedAccess>]
module Binary =

    let serialise (writer : Stream) (ser : Serialisable) =
        let serialiser = BinaryFormatter ()
        serialiser.Serialize(writer, box ser)

    let deserialise (reader : Stream) : Serialisable =
        let serialiser = BinaryFormatter ()
        serialiser.Deserialize reader |> unbox
