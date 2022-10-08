module SqlBuilder exposing (SelectQuery, build, exampleQuery)


type alias SelectQuery =
    { select : List SelectExpression
    , from : Table
    }


type SelectExpression
    = All
    | AllFromTable TableIdentifier
    | Field String


type alias TableIdentifier =
    String


type alias Table =
    { name : String
    , alias : Maybe String
    }


exampleQuery : SelectQuery
exampleQuery =
    { select =
        [ Field "f1"
        , All
        , Field "f2"
        , AllFromTable "t"
        ]
    , from = { name = "tabele", alias = Just "t" }
    }


tableToString : Table -> String
tableToString table =
    case table.alias of
        Just alias ->
            table.name ++ " " ++ alias

        Nothing ->
            table.name


columnToString : SelectExpression -> String
columnToString expression =
    case expression of
        Field fieldName ->
            fieldName

        All ->
            "*"

        AllFromTable tableIdentifier ->
            tableIdentifier ++ ".*"


build : SelectQuery -> String
build { select, from } =
    [ "SELECT"
    , List.map columnToString select
        |> String.join ", "
    , "FROM"
    , tableToString from
    ]
        |> String.join " "



{-
   select
   tables
   where
-}
