namespace Omni

open System

type Serialisable =
| String of string
| Number of string
| Bool of bool
| Object of Map<string, Serialisable>
| Array of Serialisable array

type SerPair<'a, 'ser> = ('a -> 'ser) * ('ser -> 'a)

[<RequireQualifiedAccess>]
type 'a ConvertPair =
| String of SerPair<'a, string>
| Number of SerPair<'a, string>
| Bool of SerPair<'a, bool>
| Object of SerPair<'a, Map<string, Serialisable>>
| Array of SerPair<'a, Serialisable array>
// ConvertPair also includes the ability to convert
// to and from an arbitrary Serialisable type
| Serialisable of SerPair<'a, Serialisable>

type Converter = abstract member TryGetConverter<'a> : unit -> 'a ConvertPair option

type CachingConverter =
    inherit Converter
    abstract member ConverterRequested : (Type * bool) IEvent

type ConverterCustomisation = Converter -> Converter

and ConverterCustomisationWithTypeParameter = abstract member Eval<'a> : unit -> ConverterCustomisation
