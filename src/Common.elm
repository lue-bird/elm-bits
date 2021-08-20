module Common exposing (bitsToNat)

import Arr exposing (Arr)
import InNat
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit as Bit exposing (Bit)
import MinNat
import Nat exposing (In, Min, Nat)
import Nats exposing (..)


bitsToNat : Arr (In min_ Nat53) Bit -> Nat (Min Nat0)
bitsToNat bits =
    Arr.map2
        (\power ->
            case power |> InNat.isAtLeast nat1 { lowest = nat0 } of
                Nat.EqualOrGreater powAtLeast1 ->
                    Bit.toNat
                        >> Nat.mul (nat2 |> Nat.toPower powAtLeast1)

                Nat.Below _ ->
                    Bit.toNat >> Nat.toMin
        )
        (Arr.nats nat53
            |> Arr.lowerMinLength nat0
        )
        (bits |> Arr.reverse |> Arr.lowerMinLength nat0)
        |> Arr.fold FirstToLast
            (\a b -> a |> MinNat.addMin nat0 b)
            (nat0 |> Nat.toMin)
