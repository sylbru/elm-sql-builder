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


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.class "query" ]
        [ Html.text (SqlBuilder.toString SqlBuilder.exampleQuery)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
