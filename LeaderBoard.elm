module LeaderBoard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { runners : List Runner
    , query : String
    }


type alias Runner =
    { id : Int
    , name : String
    , location : String
    }


initModel : Model
initModel =
    { runners = []
    , query = ""
    }


type Msg
    = QueryInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        QueryInput query ->
            { model | query = query }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Leader Board" ]
        , Html.form []
            [ input
                [ type_ "text"
                , onInput QueryInput
                , placeholder "Search Here"
                ]
                []
            ]
        ]
