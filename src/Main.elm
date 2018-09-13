module Main exposing (main)

import Browser
import Html exposing (Html, a, div, form, h1, h2, h3, h4, img, input, label, pre, text)
import Html.Attributes exposing (attribute, class, for, href, id, name, placeholder, src, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode
import Json.Decode.Pipeline
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder



---- MODEL ----


type alias Model =
    { searchString : String }


init : ( Model, Cmd Msg )
init =
    ( { searchString = "Blade Runner"
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | EnteredSearchString String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EnteredSearchString value ->
            ( { model
                | searchString = value
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ img [ src "%PUBLIC_URL%/logo.svg" ] []
            , form [ class "search-form" ]
                [ input
                    [ class "search-input"
                    , type_ "text"
                    , placeholder "Search Movie by Title"
                    , value model.searchString
                    , onInput EnteredSearchString
                    ]
                    []
                , input
                    [ class "search-button"
                    , type_ "submit"
                    , value "Search"
                    ]
                    []
                ]
            ]
        , viewMovieList
        ]


viewMovieList : Html Msg
viewMovieList =
    div [ class "movie-list" ]
        (List.map viewMovieItem [ { title = "Blade runner" }, { title = "Avenger" }, { title = "Avatar" } ])


viewMovieItem : { title : String } -> Html Msg
viewMovieItem movie =
    div [ class "movie-item" ]
        [ div [ class "movie-item-poster", attribute "style" "background-image: url('https://m.media-amazon.com/images/M/MV5BNzA1Njg4NzYxOV5BMl5BanBnXkFtZTgwODk5NjU3MzI@._V1_SX300.jpg');" ]
            []
        , h3 [ class "movie-item-title" ]
            [ text movie.title ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
