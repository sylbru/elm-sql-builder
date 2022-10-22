module Select exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SqlBuilder
import Test exposing (..)



--Expr.and
--    (Expr.gt (Expr.column "a") (Expr.int "42"))
--    (Expr.or (Expr.string "hey") Expr.true)


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
                    |> SqlBuilder.withColumnsIdentifiers [ "f", "g" ]
                    |> SqlBuilder.toString
                    |> Expect.equal (Ok "SELECT f, g\nFROM main_table mt")
        , test "Can’t build query if fields are not available" <|
            \_ ->
                SqlBuilder.select
                    |> SqlBuilder.withTableContaining "main_table" [ "a", "b" ]
                    |> SqlBuilder.withColumnsIdentifiers [ "a", "b", "c" ]
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
                    (SqlBuilder.select |> SqlBuilder.withColumnsIdentifiers [ "a", "b", "c" ])
                    |> Expect.equal [ "a", "b", "c" ]
        ]
