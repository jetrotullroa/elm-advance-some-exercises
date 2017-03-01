port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import LeaderBoard
import Login
import Runner


-- Main


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Main end
-- Location and


pageToHash : Page -> String
pageToHash page =
    case page of
        LeaderBoardPage ->
            "/#"

        LoginPage ->
            "#/login"

        RunnerPage ->
            "#/runner"

        NotFound ->
            "#notfound"


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "/#" ->
            LeaderBoardPage

        "#/leaderboard" ->
            LeaderBoardPage

        "#/login" ->
            LoginPage

        "#/runner" ->
            RunnerPage

        "" ->
            LeaderBoardPage

        _ ->
            NotFound


locationToMsg : Navigation.Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage



--Location and Routing END
-- model


type alias Model =
    { page : Page
    , leaderBoard : LeaderBoard.Model
    , login : Login.Model
    , runner : Runner.Model
    , token : Maybe String
    , loggedin : Bool
    }


type Page
    = NotFound
    | LeaderBoardPage
    | LoginPage
    | RunnerPage


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash

        ( leaderBoardInitModel, leaderBoardCmd ) =
            LeaderBoard.init

        ( loginInitModel, loginCmd ) =
            Login.init

        ( runnerInitModel, runnerCmd ) =
            Runner.init

        initModel =
            { page = page
            , leaderBoard = leaderBoardInitModel
            , login = loginInitModel
            , runner = runnerInitModel
            , token = Nothing
            , loggedin = False
            }

        cmds =
            Cmd.batch
                [ Cmd.map LeaderBoardMsg leaderBoardCmd
                , Cmd.map LoginMsg loginCmd
                , Cmd.map RunnerMsg runnerCmd
                ]
    in
        ( initModel, cmds )



-- update


type Msg
    = Navigate Page
    | ChangePage Page
    | LeaderBoardMsg LeaderBoard.Msg
    | LoginMsg Login.Msg
    | RunnerMsg Runner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Navigation.newUrl <| pageToHash page )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        LeaderBoardMsg msg ->
            let
                ( leaderBoardModel, cmd ) =
                    LeaderBoard.update msg model.leaderBoard
            in
                ( { model | leaderBoard = leaderBoardModel }
                , Cmd.map LeaderBoardMsg cmd
                )

        LoginMsg msg ->
            let
                ( loginModel, cmd, token ) =
                    Login.update msg model.login

                loggedin =
                    token /= Nothing
            in
                ( { model
                    | login = loginModel
                    , token = token
                    , loggedin = loggedin
                  }
                , Cmd.map LoginMsg cmd
                )

        RunnerMsg msg ->
            let
                ( runnerModel, cmd ) =
                    Runner.update msg model.runner
            in
                ( { model | runner = runnerModel }
                , Cmd.map RunnerMsg cmd
                )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                LeaderBoardPage ->
                    Html.map LeaderBoardMsg
                        (LeaderBoard.view model.leaderBoard)

                LoginPage ->
                    Html.map LoginMsg
                        (Login.view model.login)

                RunnerPage ->
                    Html.map RunnerMsg
                        (Runner.view model.runner)

                NotFound ->
                    div [ class "main" ]
                        [ h1 []
                            [ text "Page Not Found!" ]
                        ]
    in
        div []
            [ pageHeader model
            , page
            ]


pageHeader : Model -> Html Msg
pageHeader model =
    header []
        [ a [ onClick (Navigate LeaderBoardPage) ] [ text "Race Results" ]
        , ul []
            [ li []
                [ a [ onClick (Navigate RunnerPage) ] [ text "Add Runner" ]
                ]
            ]
        , ul []
            [ li []
                [ a [ onClick (Navigate LoginPage) ] [ text "Login" ]
                ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        leaderBoardSub =
            LeaderBoard.subscriptions model.leaderBoard

        loginSub =
            Login.subscriptions model.login
    in
        Sub.batch
            [ Sub.map LeaderBoardMsg leaderBoardSub
            , Sub.map LoginMsg loginSub
            ]
