module Main exposing (main)

-- elm-package install -- yes noredink/elm-decode-pipeline

import Browser
import Html exposing (Html, a, div, form, h1, h2, h3, h4, img, input, label, pre, text)
import Html.Attributes exposing (attribute, class, for, href, id, name, placeholder, src, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder



---- HTTP ----


type alias SearchResults =
    { search : List Movie
    }


type alias Movie =
    { title : String
    , year : String
    , type_ : String
    , poster : Maybe String
    }


decodeMovie : Json.Decode.Decoder Movie
decodeMovie =
    Json.Decode.succeed Movie
        |> Json.Decode.Pipeline.required "Title" Json.Decode.string
        |> Json.Decode.Pipeline.required "Year" Json.Decode.string
        |> Json.Decode.Pipeline.required "Type" Json.Decode.string
        |> Json.Decode.Pipeline.required "Poster"
            (Json.Decode.map
                (\str ->
                    case str of
                        "N/A" ->
                            Nothing

                        _ ->
                            Just str
                )
                Json.Decode.string
            )


decodeSearchResults : Json.Decode.Decoder SearchResults
decodeSearchResults =
    Json.Decode.succeed SearchResults
        |> Json.Decode.Pipeline.optional "Search"
            (Json.Decode.list decodeMovie)
            []


fetchSearchResults : String -> Cmd Msg
fetchSearchResults searchString =
    let
        url =
            Url.Builder.crossOrigin "https://www.omdbapi.com/"
                []
                [ Url.Builder.string "apikey" "2099b44a"
                , Url.Builder.string "s" searchString
                ]
    in
    decodeSearchResults
        |> Http.get url
        |> RemoteData.sendRequest
        |> Cmd.map SearchResultsReceived



---- MODEL ----


type alias Model =
    { searchString : String
    , searchResults : WebData SearchResults
    }


init : ( Model, Cmd Msg )
init =
    ( { searchString = "Blade Runner"
      , searchResults = Loading
      }
      -- , Cmd.none
    , fetchSearchResults "Blade Runner"
    )



---- UPDATE ----


type Msg
    = NoOp
    | EnteredSearchString String
    | SearchResultsReceived (WebData SearchResults)
    | ClickedSearch


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

        SearchResultsReceived searchResults ->
            ( { model
                | searchResults = searchResults
              }
            , Cmd.none
            )

        ClickedSearch ->
            ( { model | searchResults = Loading }
            , fetchSearchResults model.searchString
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ img [ src "%PUBLIC_URL%/logo.svg" ] []
            , form
                [ class "search-form"
                , onSubmit ClickedSearch
                ]
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
        , viewMovieList model.searchResults
        ]


viewMovieList : WebData SearchResults -> Html Msg
viewMovieList searchResults =
    div [ class "movie-list" ] <|
        case searchResults of
            NotAsked ->
                [ text "Please search for movies" ]

            Loading ->
                [ text "Loading" ]

            Failure _ ->
                [ text "Error!" ]

            Success results ->
                case results.search of
                    [] ->
                        [ text "no results" ]

                    movies ->
                        List.map viewMovieItem movies


viewMovieItem : Movie -> Html Msg
viewMovieItem movie =
    let
        posterAttributes =
            case movie.poster of
                Nothing ->
                    []

                Just posterUrl ->
                    [ style "backgroundImage"
                        ("url("
                            ++ posterUrl
                            ++ ")"
                        )
                    ]
    in
    div [ class "movie-item" ]
        [ div
            ([ class "movie-item-poster"
             ]
                ++ posterAttributes
            )
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
