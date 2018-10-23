namespace Omni

type Teq<'a, 'b> = internal Teq of ('a -> 'b) * ('b -> 'a)

[<RequireQualifiedAccess>]
module Teq =

    let refl<'a> : Teq<'a, 'a> = Teq (id, id)

    let castTo (Teq (f, g)) a = f a

    let castFrom (Teq (f, g)) b = g b

    let believeMe<'a, 'b> : Teq<'a, 'b> = unbox refl<'a>
