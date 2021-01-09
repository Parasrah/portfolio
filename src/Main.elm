module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (Error(..))
import Browser.Navigation exposing (Key)
import Element exposing (Color, Device, Element, fill, layout, px, text)
import Element.Background as Background
import Element.Border as Border
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
            ([ Element.height fill
             , Element.width fill
             , Background.color Style.Colors.background
             , Font.color Style.Colors.primaryFont
             ]
                ++ Style.Fonts.regular
            )
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
        [ Element.height fill
        , Element.width fill
        ]
        [ viewHeader page
        , viewCard page
            [ text "this is a test" ]
        ]


viewTimeline : Page -> Element Msg
viewTimeline page =
    Element.none


viewCard : Page -> List (Element Msg) -> Element Msg
viewCard page content =
    let
        rounded =
            5
    in
    Element.column
        [ Background.color Style.Colors.dp01
        , Element.height <| px 500
        , Element.width <| px 400
        , Font.color <| Style.Colors.primaryFont
        , Element.alignLeft
        , Element.moveRight 50
        , Element.centerY
        , Border.rounded rounded
        , Border.solid
        ]
        [ Element.el
            [ Background.color Style.Colors.dp01
            , Element.width fill
            , Element.height <| px 50
            , Border.roundEach { topLeft = rounded, topRight = rounded, bottomLeft = 0, bottomRight = 0 }
            ]
            (Element.el
                [ Element.centerY
                , Element.moveRight 10
                ]
                (Element.text "IBM Canada")
            )
        , Element.paragraph
            []
            content
        ]


viewHeader : Page -> Element Msg
viewHeader page =
    Element.row
        ([ Element.height <| px 75
         , Element.width fill
         ]
            ++ Style.Fonts.header
        )
        [ Element.el
            [ Element.moveRight 30
            , Font.size 30
            ]
            (Element.text "Brad Pfannmuller")
        , Element.row
            [ Element.height fill
            , Element.alignRight
            , Element.moveLeft 40
            , Element.spacing 35
            ]
            [ Element.image
                [ Element.alignRight ]
                { src = "/github.svg"
                , description = "Github, where I host most of my code"
                }
            , Element.image
                [ Element.alignRight ]
                { src = "/linkedin.svg"
                , description = "LinkedIn, the popular career platform"
                }
            , Element.image
                [ Element.alignRight ]
                { src = "/mail.svg"
                , description = "Send me an email!"
                }
            ]
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
