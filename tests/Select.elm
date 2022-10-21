module Select exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SqlBuilder
import Test exposing (..)


suite : Test
suite =
    describe "SELECT queries"
        [ test "Select, one table, one field" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withTable "main_table"
                    |> SqlBuilder.withColumnIdentifier "f"
                    |> SqlBuilder.toString
                    |> Expect.equal "SELECT f\nFROM main_table"
        , test "Select, one aliased table, two fields" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withAliasedTable "main_table" "mt"
                    |> SqlBuilder.withColumnIdentifier "f"
                    |> SqlBuilder.withColumnIdentifier "g"
                    |> SqlBuilder.toString
                    |> Expect.equal "SELECT f, g\nFROM main_table mt"
        , test "Select, one aliased table, two fields with `withColumnIdentifiers`" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withAliasedTable "main_table" "mt"
                    |> SqlBuilder.withColumnsIdentifiers [ "f", "g" ]
                    |> SqlBuilder.toString
                    |> Expect.equal "SELECT f, g\nFROM main_table mt"
        ]
