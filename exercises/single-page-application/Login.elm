module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { username : String
    , password : String
    }


type Msg
    = UsernameInput String
    | PasswordInput String


initModel : Model
initModel =
    { username = ""
    , password = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UsernameInput username ->
            { model | username = username }

        PasswordInput password ->
            { model | password = password }


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Please Login" ]
        , Html.form []
            [ input
                [ type_ "text"
                , onInput UsernameInput
                , placeholder "Enter username"
                ]
                []
            , input
                [ type_ "text"
                , onInput PasswordInput
                , placeholder "Enter password"
                ]
                []
            , input
                [ type_ "submit" ]
                [ text "Submit" ]
            ]
        ]
