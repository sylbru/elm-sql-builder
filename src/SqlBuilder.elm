module SqlBuilder exposing (SelectQuery, build, exampleQuery)


type alias SelectQuery =
    { select : List SelectExpression
    , from : Table
    , whereCondition : Maybe WhereExpression
    }


type SelectExpression
    = All
    | AllFromTable TableIdentifier
    | Column String


type alias TableIdentifier =
    String


type Table
    = Table String
    | TableWithAlias String String


type WhereExpression
    = Simple LiteralValue
    | Not WhereExpression
    | And WhereExpression WhereExpression
    | Or WhereExpression WhereExpression
    | Xor WhereExpression WhereExpression


type LiteralValue
    = LiteralTrue
    | LiteralFalse
    | LiteralNull
    | LiteralString String


exampleWhere : WhereExpression
exampleWhere =
    And (Simple (LiteralString "hey")) (Simple LiteralFalse)


literalValueToString : LiteralValue -> String
literalValueToString literalValue =
    case literalValue of
        LiteralTrue ->
            "TRUE"

        LiteralFalse ->
            "FALSE"

        LiteralNull ->
            "NULL"

        LiteralString string ->
            "\"" ++ string ++ "\""


exampleQuery : SelectQuery
exampleQuery =
    { select =
        [ Column "f1"
        , All
        , Column "f2"
        , AllFromTable "t"
        ]
    , from = TableWithAlias "tabele" "t"
    , whereCondition = Just exampleWhere
    }


tableToString : Table -> String
tableToString table =
    case table of
        TableWithAlias name alias ->
            name ++ " " ++ alias

        Table name ->
            name


columnToString : SelectExpression -> String
columnToString expression =
    case expression of
        Column fieldName ->
            fieldName

        All ->
            "*"

        AllFromTable tableIdentifier ->
            tableIdentifier ++ ".*"


whereToString : WhereExpression -> String
whereToString whereExpression =
    case whereExpression of
        Simple value ->
            literalValueToString value

        Not subExpression ->
            "NOT (" ++ whereToString subExpression ++ ")"

        And leftExpr rightExpr ->
            "("
                ++ whereToString leftExpr
                ++ ") AND ("
                ++ whereToString rightExpr
                ++ ")"

        Or leftExpr rightExpr ->
            "("
                ++ whereToString leftExpr
                ++ ") OR ("
                ++ whereToString rightExpr
                ++ ")"

        Xor leftExpr rightExpr ->
            "("
                ++ whereToString leftExpr
                ++ ") XOR ("
                ++ whereToString rightExpr
                ++ ")"


build : SelectQuery -> String
build { select, from, whereCondition } =
    let
        parts =
            [ "SELECT"
            , List.map columnToString select
                |> String.join ", "
            , "FROM"
            , tableToString from
            ]
                ++ (case whereCondition of
                        Just whereCondition_ ->
                            [ "WHERE", whereToString whereCondition_ ]

                        Nothing ->
                            []
                   )
    in
    String.join " " parts
