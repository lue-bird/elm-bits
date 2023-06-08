safe, typed bit arrays

# [bits](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/)

## example: id

Most id packages use an opaque `type` that hold the information.
Example from [danyx23's `Uuid`][danyx23/elm-uuid] to skim through ↓

```elm
type Uuid
    = Uuid String

toString : Uuid -> String
toString =
    \(Uuid string) -> string

fromString : String -> Maybe Uuid
fromString =
    \string ->
        if
            string
                |> Regex.contains
                    (Regex.fromString "^[0-9A-Fa-f]{8,8}-[0-9A-Fa-f]{4,4}-[1-5][0-9A-Fa-f]{3,3}-[8-9A-Ba-b][0-9A-Fa-f]{3,3}-[0-9A-Fa-f]{12,12}$"
                        |> Maybe.withDefault Regex.never
                    )
        then
            string |> Uuid |> Just

        else
            Nothing

generate : Random.Generator Uuid
generate =
    Random.map
        (\thirtyOneHexDigits ->
            Uuid
                ([ thirtyOneHexDigits |> List.take 8 |> List.map mapToHex |> String.fromList
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
                )
        )
        (Random.list 31 (Random.int 0 15))
```

with bits:

```elm
import Bit exposing (Bit)

-- from typesafe-array
import ArraySized exposing (ArraySized)
-- from typed-value
import Typed exposing (Typed, Tagged, Public, tag)
-- from bounded-nat
import N exposing (Exactly)
-- generated. See `N`'s module documentation
import N.Local exposing (N128)


random : Random.Generator Uuid
random =
    Random.map (tag Uuid)
        (ArraySized.random Bit.random n128)

type UuidTag
    = Uuid

type alias Uuid =
    Typed
        Tagged
        UuidTag
        Public
        (ArraySized Bit (Exactly (On N128)))
```

→ type-safe, clean way of storing information

🧩

- **[typesafe-array](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/)**
- [bounded-nat](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/)
- an extra: [typed-value](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/)


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


```elm
import Uuid exposing (Uuid)
import Bit exposing (Bit(..))
import Typed exposing (tag, untag)
-- from linear-direction
import Linear exposing (Direction(..))
-- from bburdette/toop
import Toop exposing (T4(..))

uuidUi : Uuid -> Html msg_
uuidUi uuid =
    (uuid |> untag) -- the raw bit array
        |> bitsToHexString
        |> Html.text

uuidFromAllBits : Uuid
uuidFromAllBits =
    -- raw bits
    ArraySized.l16 O I O I I I I O I I I I I O O O
        |> ArraySized.attach Up
            (ArraySized.l16 I I I O O O O I O I I I I O I I)
        ...
        |> tag Uuid

bitsToHexString : ArraySized Bit (In min_ (Up maxX_ To maxPlusX_)) -> String
bitsToHexString bits =
    (bits |> BitArray.toChunksOf n4) -- chunk it up
        |> ArraySized.map toHexChar
        |> ArraySized.toString

{-| Four bits represented as a hex `Char` (0-9 then a-f)
-}
bitToHexChar : ArraySized Bit (In (On N4) (Up maxTo4_ To N4)) -> Char
bitToHexChar =
    \bits ->
        case bits |> ArraySized.to4 of
            T4 O O O O ->
                '0'

            T4 O O O I ->
                '1'

            T4 O O I O ->
                '2'

            T4 O O I I ->
                '3'

            T4 O I O O ->
                '4'

            T4 O I O I ->
                '5'

            T4 O I I O ->
                '6'

            T4 O I I I ->
                '7'

            T4 I O O O ->
                '8'

            T4 I O O I ->
                '9'

            T4 I O I O ->
                'a'

            T4 I O I I ->
                'b'

            T4 I I O O ->
                'c'

            T4 I I O I ->
                'd'

            T4 I I I O ->
                'e'

            T4 I I I I ->
                'f'
```

Notice how users can
- build bit arrays _safely_ from _different sources of information_
- pattern-match conveniently and safely for different representations

## where `elm-bits` is used

- [`elm-morph`](https://package.elm-lang.org/packages/lue-bird/elm-morph/latest) can
  create a parser-builder that can even read non-byte-multiple bit counts like 7
- maybe you built something? Tell us about it ✿

----

Confused? Hyped? Hit @lue up on anything on slack

[danyx23/elm-uuid]: https://package.elm-lang.org/packages/danyx23/elm-uuid/latest/Uuid
