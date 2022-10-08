module SqlBuilder exposing (SelectQuery, build)


type alias SelectQuery =
    { select : List String
    , from : String
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
