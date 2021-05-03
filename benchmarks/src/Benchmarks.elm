module Benchmarks exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Util


main: BenchmarkProgram
main=
  Benchmark.Runner.program suite

suite: Benchmark
suite=
  describe "elm-bits"
    [ utilBenchmark
    ]

utilBenchmark: Benchmark
utilBenchmark=
  describe "util"
    [ let listOf60= List.repeat 60 "xyz"
      in
      Benchmark.compare "splitBefore: ( take, drop ) ←?→ recursive"
        "( take, drop )" (\()-> Util.splitBefore 42 listOf60)
        "recursive" (\()-> recursiveSplitAt 42 listOf60)
    ]

recursiveSplitAt:
  Int ->List element ->( List element, List element )
recursiveSplitAt index list=
  case (<) index 0 of
    True->
      ( [], list )
      
    False->
      recursiveSplitAtHelp index [] list
      |>Tuple.mapFirst List.reverse

recursiveSplitAtHelp nonNegativeIndex left right=
  case nonNegativeIndex of
    0->
      ( left, right )

    indexBiggerThan1->
      case right of
        []->
          ( left, [] )

        rightHead ::rightTail->
          recursiveSplitAtHelp (indexBiggerThan1 - 1)
            (rightHead ::left) rightTail
      

