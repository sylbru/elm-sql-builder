module SqlBuilder exposing (SelectQuery, build, exampleQuery)


type alias SelectQuery =
    { select : List Column
    , from : Table
    }


type alias Column =
    String


type alias Table =
    { name : String
    , alias : Maybe String
    }


exampleQuery : SelectQuery
exampleQuery =
    { select = [ "f1", "f2" ]
    , from = { name = "tabele", alias = Just "t" }
    }


tableToString : Table -> String
tableToString table =
    case table.alias of
        Just alias ->
            table.name ++ " " ++ alias

        Nothing ->
            table.name


build : SelectQuery -> String
build { select, from } =
    [ "SELECT"
    , String.join ", " select
    , "FROM"
    , tableToString from
    ]
        |> String.join " "



{-
   select
   tables
   where
-}
