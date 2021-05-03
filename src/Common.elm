module Common exposing (bitsToNat)

import Arr exposing (Arr)
import InNat
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit as Bit exposing (Bit)
import MinNat
import NNats exposing (..)
import Nat exposing (In, Min, Nat)
import TypeNats exposing (..)
import Typed exposing (val)


bitsToNat : Arr (In min Nat53) Bit -> Nat (Min Nat0)
bitsToNat bits =
    Arr.map2
        (\power ->
            power
                |> InNat.isAtLeast nat1
                    { min = nat0 }
                    { equalOrGreater =
                        \powAtLeast1 ->
                            Bit.to0or1
                                >> Nat.mul (nat2 |> Nat.toPower powAtLeast1)
                    , less =
                        \_ -> Bit.to0or1 >> MinNat.value
                    }
        )
        (Arr.nats nat53
            |> Arr.lowerMinLength nat0
        )
        (bits |> Arr.reverse |> Arr.lowerMinLength nat0)
        |> Arr.fold FirstToLast
            (\a b -> a |> MinNat.add b nat0)
            (nat0 |> MinNat.value)
