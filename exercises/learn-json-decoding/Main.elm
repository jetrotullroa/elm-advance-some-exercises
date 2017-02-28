module Main exposing (..)

import Html exposing (..)
import Http exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


randomJoke : Cmd Msg
randomJoke =
    let
        url =
            "http://api.icndb.com/jokes/random"

        request =
            Http.getString url

        cmd =
            Http.send Joke request
    in
        cmd


type alias Model =
    String


type Msg
    = Joke (Result Http.Error String)


initModel : Model
initModel =
    "This is a joke"


init : ( Model, Cmd Msg )
init =
    ( initModel, randomJoke )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Joke (Ok joke) ->
            ( joke, Cmd.none )

        Joke (Err err) ->
            ( (toString err), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text model ]
        ]