module Select exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    test "a" <|
        \_ -> Expect.equal "1" "1"
