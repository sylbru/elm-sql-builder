module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import SqlBuilder


type Msg
    = Msg


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


sqlQuery : SqlBuilder.SelectQuery
sqlQuery =
    { select = [ "f1", "f2" ], from = "t" }


view : Model -> Html Msg
view model =
    Html.pre
        [ Html.Attributes.class "query" ]
        [ Html.text (SqlBuilder.build sqlQuery)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
