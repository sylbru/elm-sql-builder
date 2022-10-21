module SqlBuilder exposing (SelectQuery, select, toString, withAliasedTable, withColumnIdentifier, withTable)


type SelectQuery a
    = SelectQuery
        { select : List SelectExpression
        , from : Maybe Table
        , whereCondition : Maybe Expression
        }


type SelectExpression
    = All
    | AllFromTable TableIdentifier
    | Expression Expression


type alias ColumnIdentifier =
    String


type alias TableIdentifier =
    String


type Table
    = Table String
    | TableWithAlias String String


type Expression
    = Primary PrimaryValue
    | Not Expression
    | And Expression Expression
    | Or Expression Expression
    | Xor Expression Expression


type PrimaryValue
    = Predicate Predicate
    | Eq PrimaryValue Predicate
    | Neq PrimaryValue Predicate
    | Gt PrimaryValue Predicate
    | Gte PrimaryValue Predicate
    | Lt PrimaryValue Predicate
    | Lte PrimaryValue Predicate


type Predicate
    = SimpleExpr SimpleExpr


type SimpleExpr
    = Literal LiteralValue
    | Identifier ColumnIdentifier


type LiteralValue
    = LiteralTrue
    | LiteralFalse
    | LiteralNull
    | LiteralString String
    | LiteralInt Int
    | LiteralFloat Float


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

        LiteralInt intNumber ->
            String.fromInt intNumber

        LiteralFloat floatNumber ->
            String.fromFloat floatNumber


tableToString : Table -> String
tableToString table =
    case table of
        TableWithAlias name alias ->
            name ++ " " ++ alias

        Table name ->
            name


columnToString : SelectExpression -> String
columnToString column =
    case column of
        All ->
            "*"

        AllFromTable tableIdentifier ->
            tableIdentifier ++ ".*"

        Expression expression ->
            expressionToString expression


predicateToString : Predicate -> String
predicateToString predicate =
    case predicate of
        SimpleExpr simpleExpr ->
            simpleExprToString simpleExpr


simpleExprToString : SimpleExpr -> String
simpleExprToString simpleExpr =
    case simpleExpr of
        Literal literalValue ->
            literalValueToString literalValue

        Identifier column ->
            column


primaryToString : PrimaryValue -> String
primaryToString primaryValue =
    case primaryValue of
        Predicate predicate ->
            predicateToString predicate

        Eq left right ->
            binaryOperationToString
                (primaryToString left)
                "="
                (predicateToString right)

        Neq left right ->
            binaryOperationToString
                (primaryToString left)
                "<>"
                (predicateToString right)

        Gt left right ->
            binaryOperationToString
                (primaryToString left)
                ">"
                (predicateToString right)

        Gte left right ->
            binaryOperationToString
                (primaryToString left)
                ">="
                (predicateToString right)

        Lt left right ->
            binaryOperationToString
                (primaryToString left)
                "<"
                (predicateToString right)

        Lte left right ->
            binaryOperationToString
                (primaryToString left)
                "<="
                (predicateToString right)


binaryOperationToString : String -> String -> String -> String
binaryOperationToString left operator right =
    "(" ++ left ++ " " ++ operator ++ " " ++ right ++ ")"


expressionToString : Expression -> String
expressionToString expression =
    case expression of
        Primary primary ->
            primaryToString primary

        Not subExpression ->
            "(NOT " ++ expressionToString subExpression ++ ")"

        And leftExpr rightExpr ->
            "("
                ++ expressionToString leftExpr
                ++ " AND "
                ++ expressionToString rightExpr
                ++ ")"

        Or leftExpr rightExpr ->
            "("
                ++ expressionToString leftExpr
                ++ " OR "
                ++ expressionToString rightExpr
                ++ ")"

        Xor leftExpr rightExpr ->
            "("
                ++ expressionToString leftExpr
                ++ " XOR "
                ++ expressionToString rightExpr
                ++ ")"


defaultIfEmpty : String -> List String -> List String
defaultIfEmpty default list =
    if List.isEmpty list then
        [ default ]

    else
        list


toString : SelectQuery a -> String
toString (SelectQuery selectQuery) =
    let
        selectClause =
            [ "SELECT"
            , List.map columnToString selectQuery.select
                |> defaultIfEmpty "1"
                |> String.join ", "
            ]

        fromClause =
            case selectQuery.from of
                Just from ->
                    Just
                        [ "FROM"
                        , tableToString from
                        ]

                Nothing ->
                    Nothing

        whereClause =
            case selectQuery.whereCondition of
                Just whereCondition ->
                    Just [ "WHERE", expressionToString whereCondition ]

                Nothing ->
                    Nothing

        clauses =
            List.filterMap identity
                [ Just selectClause
                , fromClause
                , whereClause
                ]
    in
    clauses
        |> List.map (String.join " ")
        |> String.join "\n"


select : SelectQuery a
select =
    SelectQuery <|
        { select = []
        , from = Nothing
        , whereCondition = Nothing
        }


withColumnIdentifier : ColumnIdentifier -> SelectQuery a -> SelectQuery a
withColumnIdentifier identifier (SelectQuery query) =
    SelectQuery <|
        { query
            | select =
                query.select
                    ++ [ Expression <| Primary <| Predicate <| SimpleExpr <| Identifier identifier ]
        }


withColumnExpression : Expression -> SelectQuery a -> SelectQuery a
withColumnExpression expression (SelectQuery query) =
    SelectQuery <|
        { query | select = query.select ++ [ Expression expression ] }


withColumnsIdentifiers : List ColumnIdentifier -> SelectQuery a -> SelectQuery a
withColumnsIdentifiers identifiers (SelectQuery query) =
    List.foldl
        (\identifier q -> q |> withColumnIdentifier identifier)
        (SelectQuery query)
        identifiers


withTable : TableIdentifier -> SelectQuery a -> SelectQuery a
withTable table (SelectQuery query) =
    SelectQuery { query | from = Just <| Table table }


withAliasedTable : TableIdentifier -> TableIdentifier -> SelectQuery a -> SelectQuery a
withAliasedTable table alias (SelectQuery query) =
    SelectQuery <|
        { query | from = Just <| TableWithAlias table alias }
