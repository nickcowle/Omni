namespace Omni

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Number =

    let ofInt = Number.Int

    let ofLong = Number.Long

    let ofFloat = Number.Float

    let tryGetInt =
        function
        | Int i -> i
        | n -> failwithf "Tried to get Int value from %A" n

    let tryGetLong =
        function
        | Long l -> l
        | n -> failwithf "Tried to get Long value from %A" n

    let tryGetFloat =
        function
        | Float f -> f
        | n -> failwithf "Tried to get Float value from %A" n
