Operate on bits and bit lists.

## example: id

Most id packages use an opaque `type` that hold the information.
Example similar to [danyx23's `Uuid`][danyx23/elm-uuid] to skim through ↓

```elm
module OpaqueId exposing (OpaqueId, generate, toString)

type OpaqueId
    = OpaqueId String

toString : OpaqueId -> String
toString =
    \(OpaqueId string) -> string

generate : Random.Generator OpaqueId
generate =
    Random.map
        (\fifteenHexDigits ->
            OpaqueId
                ([ fifteenHexDigits |> List.take 4 |> List.map mapToHex |> String.fromList
                 , "-"
                 , fifteenHexDigits |> List.drop 8 |> List.take 4 |> List.map mapToHex |> String.fromList
                 , "-"
                 , "4"
                 , fifteenHexDigits |> List.drop 12 |> List.take 3 |> List.map mapToHex |> String.fromList
                 , "-"
                 , fifteenHexDigits |> List.drop 15 |> List.take 1 |> List.map limitDigitRange8ToB |> List.map mapToHex |> String.fromList
                 ]
                    |> String.concat
                )
        )
        (Random.list 15 (Random.int 0 15))
```

with bits:

```elm
module MyId exposing (MyId(..), random)

import Bit exposing (Bit)
import Vector60

type MyId
    = MyId (Vector60 Bit) -- depending on necessary bits
```

Notice how extracting information is easy and to creating a new id can be done safely (without e.g. requiring going through decoders, parsers, validations, opaque random generators etc.).

🧩 `Vector60` is from [Chadtech/elm-vector](https://dark.elm.dmy.fr/packages/Chadtech/elm-vector/latest)
but anything will do the job, like custom codegen or [lue-bird/elm-typesafe-array](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/).
Hell even if you just use an opaque `List Bit` you'll still have it easier than with a `String`.

Bits as a universal way of representing information can be
converted from multiple types of data or its bits directly, from characters, from ints...

Bits can also be turned into a variety of representations (→ [example](https://github.com/lue-bird/elm-bits/tree/master/example))

- different string formats like human-readable (for example [michaelglass/proquint](https://package.elm-lang.org/packages/michaelglass/proquint/latest/)), less character space, hexadecimal, ...
- colors, shapes, identicons like
[coinop-logan/phace](https://package.elm-lang.org/packages/coinop-logan/phace/latest/),
[laurentpayot/minidenticons-elm](https://package.elm-lang.org/packages/laurentpayot/minidenticons-elm/latest/)
or forks of [pukkamustard/elm-identicon](https://github.com/pukkamustard/elm-identicon)
(like [dividat/elm-identicon](https://package.elm-lang.org/packages/dividat/elm-identicon/latest/)),
[`miniBill/elm-avataaars`](https://dark.elm.dmy.fr/packages/miniBill/elm-avataaars/latest/),
...

## where `elm-bits` is used

- [`elm-morph`](https://package.elm-lang.org/packages/lue-bird/elm-morph/latest) can
  create a parser-builder that can even read non-byte-multiple bit counts like 7
- maybe you built something? Tell me about it ✿



----

Confused? Hyped? Hit @lue up on anything on slack

[danyx23/elm-uuid]: https://package.elm-lang.org/packages/danyx23/elm-uuid/latest/Uuid
