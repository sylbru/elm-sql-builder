module Select exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SqlBuilder
import Test exposing (..)


suite : Test
suite =
    test "Simple select, one table, two fields" <|
        \_ ->
            SqlBuilder.select
                |> SqlBuilder.withAliasedTable "main_table" "mt"
                |> SqlBuilder.withColumnIdentifier "f"
                |> SqlBuilder.withColumnIdentifier "g"
                |> SqlBuilder.toString
                |> Expect.equal "SELECT f, g\nFROM main_table mt"
