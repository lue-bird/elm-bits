## elm-bits
Bits are a good way of storing information. The goal of this package is to enable transporting information as bits in elm.

## Example: id

Current id packages have different ways of showing the contained information. This is for example how [danyx23's Uuid][danyx23/elm-uuid#toString] does it:

```elm
module CurrentUuid exposing (CurrentUuid, toString, fromString)

type CurrentUuid =
    CurrentUuid String

fromString : String -> Maybe CurrentUuid
fromString string =
    if
        Regex.contains
            (Regex.fromString "^[0-9A-Fa-f]{8,8}-[0-9A-Fa-f]{4,4}-[1-5][0-9A-Fa-f]{3,3}-[8-9A-Ba-b][0-9A-Fa-f]{3,3}-[0-9A-Fa-f]{12,12}$"
                |> Maybe.withDefault Regex.never
            )
            string
    then
        Just (CurrentUuid string)

    else
        Nothing

toString : CurrentUuid -> String
toString =
    \(CurrentUuid string) -> string

generate : Random.Generator CurrentUuid
generate =
    Random.map
        (\thirtyOneHexDigits ->
            [ thirtyOneHexDigits |> List.take 8 |> List.map mapToHex |> String.fromList
            , "-"
            , thirtyOneHexDigits |> List.drop 8 |> List.take 4 |> List.map mapToHex |> String.fromList
            , "-"
            , "4"
            , thirtyOneHexDigits |> List.drop 12 |> List.take 3 |> List.map mapToHex |> String.fromList
            , "-"
            , thirtyOneHexDigits |> List.drop 15 |> List.take 1 |> List.map limitDigitRange8ToB |> List.map mapToHex |> String.fromList
            , thirtyOneHexDigits |> List.drop 16 |> List.take 3 |> List.map mapToHex |> String.fromList
            , "-"
            , thirtyOneHexDigits |> List.drop 19 |> List.take 12 |> List.map mapToHex |> String.fromList
            ]
                |> String.concat
                |> CurrentUuid
        )
        (Random.list 31 (Random.int 0 15))
```

With bits:

```elm
module Uuid exposing (Uuid, generate)

import Typed -- from lue-bird/elm-typed-value
import Arr -- from lue-bird/elm-typesafe-array

-- from lue-bird/elm-bounded-nat
import TypeNats exposing (Nat128)
import Nat exposing (Only)

type alias Uuid =
    Typed Tagged UuidTag Public (Arr (Only Nat128) Bit)

type UuidTag = UuidTag Never

generate : Random.Generator Uuid
generate =
    Typed.serialize (Bits.serialize nat128)
```

package links:

- [typesafe-array](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/)
    - which uses [bounded-nat](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/)
- just as an extra: [typed-value](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/)

→ type-safety & a cleaner way of storing information

Bits – as a universal way of representing information – could then be

- represented in a varienty of ways (→ [in action](https://lue-bird.github.io/elm-bits/try/))

    - different string formats (human readable (for example [michaelglass/proquint](https://package.elm-lang.org/packages/michaelglass/proquint/latest/), less character space, hexadecimal, ...)
    - colors, shapes, identicons ([coinop-logan/phace][coinop-logan/phace] or forks of [pukkamustard/elm-identicon](pukkamustard/elm-identicon) (e.g. [dividat/elm-identicon][dividat/elm-identicon])), ...

- converted from multiple types of data

    - from bits directly, from characters, from ints...

```elm
import Uuid exposing (Uuid)
import Lue.Bit exposing (Bit(..))
import Typed exposing (tag, val)

viewUuid : Uuid -> Html msg
viewUuid uuid =
    val uuid -- our bits
        |> bitsToReadableString
        |> Html.text

uuidFromAllBits : Uuid
uuidFromAllBits =
    -- raw bits
    Arr.from16 O I O I I I I O I I I I I O O O
        |> Arr.extendOnly nat16
            (Arr.from16 I I I O O O O I O I I I I O I I)
        -- ...
        |> tag

uuidFromChars : Uuid
uuidFromChars =
    let
        c char =
            Arr.extendOnly nat20 (charToBits char)
    in
    Arr.empty
        -- the first 120 bits
        |> c '骖' |> c '򥔤' |> c '򚔤'
        |> c '򒒔' |> c '񉉉' |> c '𥩒'
        -- the last 8 bits
        |> Arr.extendOnly nat8
            (Arr.from8 O I O I I I I O)
        |> tag

-- from another package
charToBits : Char -> Arr (In Nat20 (Nat20Plus a)) Bit
bitsToReadableString : Arr (In min max) Bit -> String
```

Notice how all of these can build `Uuid`s **safely** & from different sources of information.

→ This package currently contains the building blocks in relation to bits. They can be serialized, generated, modified & read.

[coinop-logan/phace]: https://package.elm-lang.org/packages/coinop-logan/phace/latest/
[pukkamustard/elm-identicon]: https://github.com/pukkamustard/elm-identicon
[dividat/elm-identicon]: https://package.elm-lang.org/packages/dividat/elm-identicon/latest/
[danyx23/elm-uuid#toString]: https://package.elm-lang.org/packages/danyx23/elm-uuid/latest/Uuid#toString
