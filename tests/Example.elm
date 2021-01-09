module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Web Portfolio"
        [ test "dummy test" <|
            \n -> Expect.pass
        ]
