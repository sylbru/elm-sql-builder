module SqlBuilder exposing (SelectQuery, exampleQuery, toString)


type alias SelectQuery =
    { select : List SelectExpression
    , from : Maybe Table
    , whereCondition : Maybe WhereExpression
    }


type SelectExpression
    = All
    | AllFromTable TableIdentifier
    | Column ColumnIdentifier
    | Expression SimpleExpr


type alias ColumnIdentifier =
    String


type alias TableIdentifier =
    String


type Table
    = Table String
    | TableWithAlias String String


type WhereExpression
    = Primary PrimaryValue
    | Not WhereExpression
    | And WhereExpression WhereExpression
    | Or WhereExpression WhereExpression
    | Xor WhereExpression WhereExpression


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
columnToString expression =
    case expression of
        Column fieldName ->
            fieldName

        All ->
            "*"

        AllFromTable tableIdentifier ->
            tableIdentifier ++ ".*"

        Expression simpleExpression ->
            simpleExprToString simpleExpression


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


whereToString : WhereExpression -> String
whereToString whereExpression =
    case whereExpression of
        Primary primary ->
            primaryToString primary

        Not subExpression ->
            "(NOT " ++ whereToString subExpression ++ ")"

        And leftExpr rightExpr ->
            "("
                ++ whereToString leftExpr
                ++ " AND "
                ++ whereToString rightExpr
                ++ ")"

        Or leftExpr rightExpr ->
            "("
                ++ whereToString leftExpr
                ++ " OR "
                ++ whereToString rightExpr
                ++ ")"

        Xor leftExpr rightExpr ->
            "("
                ++ whereToString leftExpr
                ++ " XOR "
                ++ whereToString rightExpr
                ++ ")"


toString : SelectQuery -> String
toString selectQuery =
    let
        selectClause =
            [ "SELECT"
            , List.map columnToString selectQuery.select
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
                    Just [ "WHERE", whereToString whereCondition ]

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
        |> List.concat
        |> String.join "\n"


exampleQuery : SelectQuery
exampleQuery =
    { select = [ Expression <| Identifier "id" ]
    , from = Just <| Table "t"
    , whereCondition = Just exampleWhere
    }


exampleWhere : WhereExpression
exampleWhere =
    Primary
        (Gt
            (Predicate (SimpleExpr (Identifier "f")))
            (SimpleExpr (Literal (LiteralInt 3)))
        )


select : SelectQuery
select =
    { select = [ All ]
    , from = Nothing
    , whereCondition = Nothing
    }
