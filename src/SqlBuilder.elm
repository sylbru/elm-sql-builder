module SqlBuilder exposing (SelectQuery, build, exampleQuery)


type alias SelectQuery =
    { select : List SelectExpression
    , from : Table
    , whereCondition : Maybe WhereExpression
    }


type SelectExpression
    = All
    | AllFromTable TableIdentifier
    | Column ColumnIdentifier


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
            primaryToString left
                ++ " = "
                ++ predicateToString right


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


exampleQuery : SelectQuery
exampleQuery =
    { select = [ All ]
    , from = Table "t"
    , whereCondition = Just exampleWhere
    }


exampleWhere : WhereExpression
exampleWhere =
    Primary
        (Eq
            (Predicate (SimpleExpr (Identifier "f")))
            (SimpleExpr (Literal (LiteralInt 3)))
        )
