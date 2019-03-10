namespace Omni

open System

type Number =
| Long of int64
| Float of float

type Serialisable =
| String of string
| Number of Number
| Bool of bool
| Object of (string * Serialisable) list
| Array of Serialisable array

type 'a ConvertPair = ('a -> Serialisable) * (Serialisable -> 'a)

type Converter = abstract member TryGetConverter<'a> : unit -> 'a ConvertPair option

type CachingConverter =
    inherit Converter
    abstract member ConverterRequested : (Type * bool) IEvent

type ConverterCustomisation = Converter -> Converter

and ConverterCustomisationWithTypeParameter = abstract member Eval<'a> : unit -> ConverterCustomisation
