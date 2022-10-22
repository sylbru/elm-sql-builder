module SqlBuilder exposing (SelectQuery, availableFields, requiredFields, select, toString, withAliasedTable, withColumnIdentifier, withColumnsIdentifiers, withTable, withTableContaining)


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
    | TableWithFields String (List ColumnIdentifier)


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


type alias Error =
    String


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

        TableWithFields name _ ->
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
            binaryOperationToString (primaryToString left) "=" (predicateToString right)

        Neq left right ->
            binaryOperationToString (primaryToString left) "<>" (predicateToString right)

        Gt left right ->
            binaryOperationToString (primaryToString left) ">" (predicateToString right)

        Gte left right ->
            binaryOperationToString (primaryToString left) ">=" (predicateToString right)

        Lt left right ->
            binaryOperationToString (primaryToString left) "<" (predicateToString right)

        Lte left right ->
            binaryOperationToString (primaryToString left) "<=" (predicateToString right)


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
            binaryOperationToString (expressionToString leftExpr) "AND" (expressionToString rightExpr)

        Or leftExpr rightExpr ->
            binaryOperationToString (expressionToString leftExpr) "OR" (expressionToString rightExpr)

        Xor leftExpr rightExpr ->
            binaryOperationToString (expressionToString leftExpr) "XOR" (expressionToString rightExpr)


defaultIfEmpty : String -> List String -> List String
defaultIfEmpty default list =
    if List.isEmpty list then
        [ default ]

    else
        list


toString : SelectQuery a -> Result Error String
toString (SelectQuery selectQuery) =
    if isValid (SelectQuery selectQuery) then
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
        Ok <|
            (clauses
                |> List.map (String.join " ")
                |> String.join "\n"
            )

    else
        Err "Can’t build query"


isValid : SelectQuery a -> Bool
isValid ((SelectQuery selectQuery) as wrapped) =
    let
        tables =
            case selectQuery.from of
                Just fromTable ->
                    [ fromTable ]

                Nothing ->
                    []

        tableHasFieldsInfo : Table -> Bool
        tableHasFieldsInfo table =
            case table of
                TableWithFields _ _ ->
                    True

                _ ->
                    False
    in
    -- At least one table has no field info,
    -- so we can’t know if fields are unavailable
    List.any (not << tableHasFieldsInfo) tables
        || -- All tables have field info, we can check if
           -- all required fields are available
           List.all
            (\fieldInSelect -> List.member fieldInSelect (availableFields wrapped))
            (requiredFields wrapped)


availableFields : SelectQuery a -> List ColumnIdentifier
availableFields (SelectQuery selectQuery) =
    let
        from =
            selectQuery.from
    in
    case from of
        Just (TableWithFields _ fields) ->
            fields

        _ ->
            []


requiredFields : SelectQuery a -> List ColumnIdentifier
requiredFields (SelectQuery query) =
    List.concat (List.map fieldsInSelectExpression query.select)


fieldsInSelectExpression : SelectExpression -> List ColumnIdentifier
fieldsInSelectExpression selectExpression =
    case selectExpression of
        Expression expression ->
            fieldsInExpression expression

        _ ->
            []


fieldsInExpression : Expression -> List ColumnIdentifier
fieldsInExpression expression =
    case expression of
        Primary primaryValue ->
            fieldsInPrimaryValue primaryValue

        Not subExpression ->
            fieldsInExpression subExpression

        And leftExpression rightExpression ->
            List.append (fieldsInExpression leftExpression) (fieldsInExpression rightExpression)

        Or leftExpression rightExpression ->
            List.append (fieldsInExpression leftExpression) (fieldsInExpression rightExpression)

        Xor leftExpression rightExpression ->
            List.append (fieldsInExpression leftExpression) (fieldsInExpression rightExpression)


fieldsInPrimaryValue : PrimaryValue -> List ColumnIdentifier
fieldsInPrimaryValue primaryValue =
    case primaryValue of
        Predicate predicate ->
            fieldInPredicate predicate

        Eq subValue predicate ->
            List.append (fieldsInPrimaryValue subValue) (fieldInPredicate predicate)

        Neq subValue predicate ->
            List.append (fieldsInPrimaryValue subValue) (fieldInPredicate predicate)

        Gt subValue predicate ->
            List.append (fieldsInPrimaryValue subValue) (fieldInPredicate predicate)

        Gte subValue predicate ->
            List.append (fieldsInPrimaryValue subValue) (fieldInPredicate predicate)

        Lt subValue predicate ->
            List.append (fieldsInPrimaryValue subValue) (fieldInPredicate predicate)

        Lte subValue predicate ->
            List.append (fieldsInPrimaryValue subValue) (fieldInPredicate predicate)


fieldInPredicate : Predicate -> List ColumnIdentifier
fieldInPredicate predicate =
    case predicate of
        SimpleExpr (Identifier columnIdentifier) ->
            [ columnIdentifier ]

        _ ->
            []


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


withTableContaining : TableIdentifier -> List ColumnIdentifier -> SelectQuery a -> SelectQuery a
withTableContaining table columns (SelectQuery query) =
    SelectQuery <|
        { query
            | from = Just <| TableWithFields table columns
        }
