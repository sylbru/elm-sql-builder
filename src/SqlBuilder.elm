module SqlBuilder exposing (SelectQuery, build, exampleQuery)


type alias SelectQuery =
    { select : List SelectExpression
    , from : Table
    }


type SelectExpression
    = All
    | AllFromTable TableIdentifier
    | Column String


type alias TableIdentifier =
    String


type alias Table =
    { name : String
    , alias : Maybe String
    }


exampleQuery : SelectQuery
exampleQuery =
    { select =
        [ Column "f1"
        , All
        , Column "f2"
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
        Column fieldName ->
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
