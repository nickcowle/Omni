namespace Omni

open System

type Serialisable =
| String of string
| Int32 of int32
| Int64 of int64
| Float of float
| Bool of bool
| Object of Map<string, Serialisable>
| Array of Serialisable array
// Arrays for performance optimisations
| StringArray of string array
| Int32Array of int32 array
| Int64Array of int64 array
| FloatArray of float array
| BoolArray of bool array

type 'a ConvertPair = ('a -> Serialisable) * (Serialisable -> 'a)

type Converter = abstract member TryGetConverter<'a> : unit -> 'a ConvertPair option

type CachingConverter =
    inherit Converter
    abstract member ConverterRequested : (Type * bool) IEvent

type ConverterForTypeEvaluator<'ret> = abstract member Eval : (Converter -> 'a ConvertPair) -> 'ret
type ConverterForTypeCrate = abstract member Apply : ConverterForTypeEvaluator<'ret> -> 'ret

type ConvertPairCrateEvaluator<'ret> = abstract member Eval : 'a ConvertPair -> 'ret
type ConvertPairCrate = abstract member Apply : ConvertPairCrateEvaluator<'ret> -> 'ret

type ConverterCustomisation =
| ForType of ConverterForTypeCrate
| Custom of (Converter -> Converter)
