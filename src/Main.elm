module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (Error(..))
import Browser.Navigation exposing (Key)
import Element exposing (Color, Device, Element, fill, layout, px)
import Element.Background as Background
import Element.Font as Font
import Json.Decode as D exposing (Decoder, Value, int)
import Json.Decode.Pipeline exposing (required)
import Style.Colors
import Style.Fonts
import Url exposing (Url)



{- Model -}


type alias Model =
    Result String Page


type alias Page =
    { key : Key
    , device : Device
    }


type alias Flags =
    { width : Int
    , height : Int
    }



{- Message -}


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest



{- Init -}


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flagsJson url key =
    case D.decodeValue flagsDecoder flagsJson of
        Ok flags ->
            let
                device =
                    Element.classifyDevice { height = flags.height, width = flags.width }
            in
            ( Ok { key = key, device = device }, Cmd.none )

        Err _ ->
            ( Err "failed to parse flags", Cmd.none )



{- Update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ok page ->
            case msg of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model, Browser.Navigation.pushUrl page.key (Url.toString url) )

                        Browser.External href ->
                            ( model, Browser.Navigation.load href )

                UrlChanged _ ->
                    ( model, Cmd.none )

        Err _ ->
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
            [ Element.height fill
            , Element.width fill
            , Background.color Style.Colors.background
            ]
            (case model of
                Err err ->
                    Element.el
                        [ Font.color Style.Colors.primaryFont ]
                        (Element.text err)

                Ok page ->
                    viewPage page
            )
        ]
    }


viewPage : Page -> Element Msg
viewPage page =
    Element.column
        []
        [ viewCard page Style.Colors.dp01
        , viewCard page Style.Colors.dp02
        , viewCard page Style.Colors.dp03
        , viewCard page Style.Colors.dp04
        , viewCard page Style.Colors.dp06
        , viewCard page Style.Colors.dp08
        ]


viewTimeline : Page -> Element Msg
viewTimeline page =
    Element.none


viewCard : Page -> Color -> Element Msg
viewCard page background =
    Element.column
        ([ Background.color background
         , Element.height <| px 200
         , Element.width <| px 200
         , Font.color <| Style.Colors.primaryFont
         ]
            ++ Style.Fonts.regular
        )
        [ Element.text "Hello World"
        ]



{- Json -}


flagsDecoder : Decoder Flags
flagsDecoder =
    D.succeed Flags
        |> required "width" int
        |> required "height" int



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
