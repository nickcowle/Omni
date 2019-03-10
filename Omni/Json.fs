namespace Omni

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.IO

[<RequireQualifiedAccess>]
module Json =

    let rec private fromSerialisable (ser : Serialisable) : JToken =
        match ser with
        | String s -> JValue.FromObject s
        | Number (Long l) -> JValue.FromObject l
        | Number (Float f) -> JValue.FromObject f
        | Bool   b -> JValue.FromObject b
        | Object xs ->
            let o = JObject ()
            xs |> Seq.iter (fun (k, v) -> o.Add(k, fromSerialisable v))
            o :> JToken
        | Array a ->
            let arr = JArray ()
            a |> Array.iter (fromSerialisable >> arr.Add)
            arr :> JToken

    let rec private toSerialisable (jt : JToken) : Serialisable =
        match jt with
        | :? JObject as jt ->
            jt.Children<JProperty> ()
            |> Seq.map (fun jt -> jt.Name, jt.Value |> toSerialisable)
            |> List.ofSeq
            |> Serialisable.Object
        | :? JValue as jt ->
            match jt.Type with
            | JTokenType.Integer ->
                jt.Value |> unbox<int64> |> Number.Long |> Serialisable.Number
            | JTokenType.Float ->
                jt.Value |> unbox<float> |> Number.Float |> Serialisable.Number
            | JTokenType.String ->
                (jt.Value |> unbox<string>) |> Serialisable.String
            | JTokenType.Boolean ->
                (jt.Value |> unbox<bool>) |> Serialisable.Bool
            | _ -> failwithf "Could not convert JToken into Serialisable - did not recognise JTokenType %A" jt.Type
        | :? JArray as jt ->
            jt.Children () |> Seq.map toSerialisable |> Array.ofSeq |> Serialisable.Array
        | _ ->
            failwithf "Could not convert JToken into Serialisable - did not recognise JToken type %s" (jt.GetType().Name)

    let serialise (writer : TextWriter) (ser : Serialisable) =
        use writer = new JsonTextWriter(writer)
        writer.Formatting <- Formatting.Indented
        let jt = fromSerialisable ser
        jt.WriteTo writer

    let deserialise (reader : TextReader) : Serialisable =
        use reader = new JsonTextReader(reader)
        JToken.ReadFrom reader |> toSerialisable
