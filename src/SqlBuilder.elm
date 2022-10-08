module SqlBuilder exposing (SelectQuery, build, exampleQuery)


type alias SelectQuery =
    { select : List Column
    , from : Table
    }


type alias Column =
    String


type alias Table =
    String


table =
    "table"


exampleQuery : SelectQuery
exampleQuery =
    { select = [ "f1", "f2" ]
    , from = table
    }


build : SelectQuery -> String
build { select, from } =
    [ "SELECT"
    , String.join ", " select
    , "FROM"
    , from
    ]
        |> String.join " "



{-
   select
   tables
   where
-}
