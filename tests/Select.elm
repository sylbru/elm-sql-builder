module Select exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SqlBuilder
import Test exposing (..)


toString : Test
toString =
    describe "toString"
        [ test "Select, one table, one field" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withTable "main_table"
                    |> SqlBuilder.withColumnIdentifier "f"
                    |> SqlBuilder.toString
                    |> Expect.equal (Ok "SELECT f\nFROM main_table")
        , test "Select, one aliased table, two fields" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withAliasedTable "main_table" "mt"
                    |> SqlBuilder.withColumnIdentifier "f"
                    |> SqlBuilder.withColumnIdentifier "g"
                    |> SqlBuilder.toString
                    |> Expect.equal (Ok "SELECT f, g\nFROM main_table mt")
        , test "Select, one aliased table, two fields with `withColumnIdentifiers`" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withAliasedTable "main_table" "mt"
                    |> SqlBuilder.withColumnIdentifiers [ "f", "g" ]
                    |> SqlBuilder.toString
                    |> Expect.equal (Ok "SELECT f, g\nFROM main_table mt")
        , test "Select with literal values" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withColumnExpression SqlBuilder.true
                    |> SqlBuilder.withColumnExpression SqlBuilder.false
                    |> SqlBuilder.withColumnExpression SqlBuilder.null
                    |> SqlBuilder.withColumnExpression (SqlBuilder.string "test")
                    |> SqlBuilder.withColumnExpression (SqlBuilder.int 42)
                    |> SqlBuilder.withColumnExpression (SqlBuilder.float 3.14)
                    |> SqlBuilder.toString
                    |> Expect.equal (Ok "SELECT TRUE, FALSE, NULL, \"test\", 42, 3.14")
        , test "Select with expression and field" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withColumnExpression (SqlBuilder.int 33)
                    |> SqlBuilder.withColumnIdentifier "some_field"
                    |> SqlBuilder.withTable "t"
                    |> SqlBuilder.toString
                    |> Expect.equal (Ok "SELECT 33, some_field\nFROM t")
        , test "Can’t build query if fields are not available" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withTableContaining "main_table" [ "a", "b" ]
                    |> SqlBuilder.withColumnIdentifiers [ "a", "b", "c" ]
                    |> SqlBuilder.toString
                    |> Expect.equal (Err "Can’t build query")
        ]


availableFields : Test
availableFields =
    describe "availableFields"
        [ test "Available fields for table" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withTableContaining "main_table" [ "a", "b" ]
                    |> SqlBuilder.availableFields
                    |> Expect.equal [ "a", "b" ]
        ]


requiredFields : Test
requiredFields =
    describe "requiredFields"
        [ test "Required fields for table based on select" <|
            \_ ->
                SqlBuilder.requiredFields
                    (SqlBuilder.select |> SqlBuilder.withColumnIdentifiers [ "a", "b", "c" ])
                    |> Expect.equal [ "a", "b", "c" ]
        ]
