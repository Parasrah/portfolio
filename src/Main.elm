module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (Error(..))
import Browser.Navigation exposing (Key)
import Element exposing (Element, fill, height, layout, width)
import Element.Background as Background
import Json.Decode exposing (Value)
import Style.Colors
import Url exposing (Url)



{- Model -}


type alias Model =
    { key : Key }



{- Message -}


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest



{- Init -}


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flagsJson url key =
    ( { key = key }, Cmd.none )



{- Update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged _ ->
            ( model, Cmd.none )



{- Subscription -}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{- View -}


view : Model -> Document Msg
view model =
    { title = "Brad Pfannmuller"
    , body =
        [ layout
            [ height fill
            , width fill
            , Background.color Style.Colors.background
            ]
            (viewPage model)
        ]
    }


viewPage : Model -> Element Msg
viewPage model =
    Element.none



{- Program -}


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \u -> UrlChanged u
        , onUrlRequest = \r -> LinkClicked r
        }
