port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (Error(..))
import Browser.Events
import Browser.Navigation exposing (Key)
import Element exposing (Attribute, Color, Device, Element, fill, layout, px, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Json.Decode as D exposing (Decoder, Value, int)
import Json.Decode.Pipeline exposing (required)
import Style.Colors
import Style.Fonts
import Task
import Url exposing (Url)



{- Model -}


type Model
    = Page IPage
    | JsonError D.Error
    | DomError Browser.Dom.Error


type alias IPage =
    { key : Key
    , dimensions : Dimensions
    , device : Device
    , cards : List Card
    }


type alias Flags =
    { dimensions : Dimensions
    }


type alias Card =
    { content : List (Element Msg)
    , title : String
    , shown : Bool
    , top : Maybe Int
    }


type alias CardInfo =
    { content : List (Element Msg)
    , title : String
    }


type alias Dimensions =
    { scene : Scene
    , viewport : Viewport
    }


type alias Scene =
    { height : Float
    , width : Float
    }


type alias Viewport =
    { height : Float
    , width : Float
    , x : Float
    , y : Float
    }



{- Message -}


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest
    | OnDimensions Dimensions
    | OnResize
    | OnScroll
    | OnCardLocations (Result Browser.Dom.Error (List Browser.Dom.Element))



{- Ports -}


port onScroll : (String -> msg) -> Sub msg



{- Init -}


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flagsJson url key =
    case D.decodeValue flagsDecoder flagsJson of
        Ok flags ->
            let
                { dimensions } =
                    flags

                { viewport } =
                    dimensions

                device =
                    Element.classifyDevice
                        { height = ceiling viewport.height
                        , width = ceiling viewport.width
                        }

                cards =
                    List.map
                        (\c ->
                            { content = c.content
                            , title = c.title
                            , shown = True
                            , top = Nothing
                            }
                        )
                        cardInfo
            in
            ( Page
                { key = key
                , device = device
                , dimensions = flags.dimensions
                , cards = cards
                }
            , fetchCardLocations cards
            )

        Err err ->
            ( JsonError err, Cmd.none )



{- Update -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Page page ->
            case msg of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model, Browser.Navigation.pushUrl page.key (Url.toString url) )

                        Browser.External href ->
                            ( model, Browser.Navigation.load href )

                UrlChanged _ ->
                    ( model, Cmd.none )

                OnDimensions dimensions ->
                    let
                        { viewport } =
                            page.dimensions
                    in
                    ( Page
                        { page
                            | dimensions = dimensions
                            , device =
                                Element.classifyDevice
                                    { height = ceiling viewport.height
                                    , width = ceiling viewport.width
                                    }
                        }
                    , Cmd.none
                    )

                OnResize ->
                    ( model
                    , Cmd.batch
                        [ fetchCardLocations page.cards
                        , Task.perform OnDimensions Browser.Dom.getViewport
                        ]
                    )

                -- only need to update dimensions, top of browser elements haven't
                -- moved
                OnScroll ->
                    ( model
                    , Task.perform OnDimensions Browser.Dom.getViewport
                    )

                OnCardLocations res ->
                    case res of
                        Ok locations ->
                            let
                                cards =
                                    List.map2
                                        (\card location ->
                                            { card | top = Just <| ceiling location.element.y }
                                        )
                                        page.cards
                                        locations
                            in
                            ( Page { page | cards = cards }, Cmd.none )

                        Err err ->
                            ( DomError err, Cmd.none )

        _ ->
            ( model, Cmd.none )


fetchCardLocations : List Card -> Cmd Msg
fetchCardLocations cards =
    let
        task =
            Task.sequence
                (List.indexedMap
                    (\i _ ->
                        let
                            id =
                                cardId i
                        in
                        Browser.Dom.getElement id
                    )
                    cards
                )
    in
    Task.attempt OnCardLocations task



{- Subscription -}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> OnResize)
        , onScroll (\_ -> OnScroll)
        ]



{- View -}


view : Model -> Document Msg
view model =
    { title = "Brad Pfannmuller"
    , body =
        [ layout
            ([ Element.width fill
             , Background.color Style.Colors.background
             , Font.color Style.Colors.primaryFont
             ]
                ++ Style.Fonts.regular
            )
            (case model of
                JsonError err ->
                    Element.el
                        [ Font.color Style.Colors.primaryFont ]
                        (Element.text "failed to parse json")

                DomError err ->
                    Element.el
                        [ Font.color Style.Colors.primaryFont ]
                        (Element.text "failed to find cards")

                Page page ->
                    viewPage page
            )
        ]
    }


viewPage : IPage -> Element Msg
viewPage page =
    Element.column
        []
        [ viewHeader page
        , viewTimeline page
        ]


viewTimeline : IPage -> Element Msg
viewTimeline page =
    Element.column
        [ Element.height fill
        , Element.width fill
        , Element.spacing 50
        , Element.Region.mainContent
        ]
        (List.indexedMap
            (\i card ->
                let
                    content =
                        viewCard
                            [ cardAttr i ]
                            card.title
                            card.content

                    even =
                        modBy 2 i == 0

                    rowElements =
                        [ Element.el
                            [ Element.width <| Element.fillPortion 6
                            ]
                            content
                        , Element.el
                            [ Element.width <| Element.fillPortion 4 ]
                            Element.none
                        ]
                in
                Element.row
                    [ Element.padding 50 ]
                    (if even then
                        rowElements

                     else
                        List.reverse rowElements
                    )
            )
            page.cards
        )



-- TODO: want to have tags on each card
-- TODO: make card slightly larger when hovered (animate)
-- TODO: render cards when they are scrolled 30% into page
-- TODO: if there isn't 30% of page left, render when you can


viewCard : List (Attribute Msg) -> String -> List (Element Msg) -> Element Msg
viewCard attrs title content =
    let
        rounded =
            5
    in
    Element.column
        ([ Background.color Style.Colors.dp01
         , Element.width fill
         , Font.color <| Style.Colors.primaryFont
         , Element.alignLeft
         , Element.centerY
         , Border.rounded rounded
         , Border.solid
         ]
            ++ attrs
        )
        [ Element.el
            [ Background.color Style.Colors.dp01
            , Element.width fill
            , Element.height <| px 50
            , Border.roundEach { topLeft = rounded, topRight = rounded, bottomLeft = 0, bottomRight = 0 }
            ]
            (Element.el
                [ Element.centerY
                , Element.moveRight 30
                ]
                (Element.text title)
            )
        , Element.textColumn
            [ Element.padding 15
            , Font.size 18
            , Element.width fill
            , Element.spacing 20
            ]
            content
        ]


viewHeader : IPage -> Element Msg
viewHeader page =
    Element.row
        ([ Element.height <| px 75
         , Element.width fill
         , Element.Region.navigation
         ]
            ++ Style.Fonts.header
        )
        [ Element.link
            [ Element.Region.heading 1
            , Element.moveRight 30
            , Font.size 30
            ]
            { url = "/"
            , label = Element.text "Brad Pfannmuller"
            }
        , Element.row
            [ Element.height fill
            , Element.alignRight
            , Element.moveLeft 40
            , Element.spacing 35
            ]
            [ Element.newTabLink
                []
                { url = "https://github.com/parasrah"
                , label =
                    Element.image
                        [ Element.alignRight ]
                        { src = "/github.svg"
                        , description = "Github, where I host most of my code"
                        }
                }
            , Element.newTabLink
                []
                { url = "https://www.linkedin.com/in/brad-pfannmuller/"
                , label =
                    Element.image
                        [ Element.alignRight ]
                        { src = "/linkedin.svg"
                        , description = "LinkedIn, the popular career platform"
                        }
                }
            , Element.link
                []
                { url = "mailto:jobs@parasrah.com"
                , label =
                    Element.image
                        [ Element.alignRight ]
                        { src = "/mail.svg"
                        , description = "Send me an email!"
                        }
                }
            ]
        ]


fadeInLeft : Attribute Msg
fadeInLeft =
    Element.htmlAttribute <| Html.Attributes.class "animate__animated animate__fadeInLeft"


fadeInRight : Attribute Msg
fadeInRight =
    Element.htmlAttribute <| Html.Attributes.class "animate__animated animate__fadeInRight"


cardId : Int -> String
cardId i =
    "card-" ++ String.fromInt i


cardAttr : Int -> Attribute Msg
cardAttr i =
    let
        id =
            cardId i
    in
    Element.htmlAttribute <| Html.Attributes.id id



{- Json -}


flagsDecoder : Decoder Flags
flagsDecoder =
    D.succeed Flags
        |> required "dimensions" dimensionsDecoder


dimensionsDecoder : Decoder Dimensions
dimensionsDecoder =
    D.succeed Dimensions
        |> required "scene" sceneDecoder
        |> required "viewport" viewportDecoder


sceneDecoder : Decoder Scene
sceneDecoder =
    D.succeed Scene
        |> required "height" D.float
        |> required "width" D.float


viewportDecoder : Decoder Viewport
viewportDecoder =
    D.succeed Viewport
        |> required "height" D.float
        |> required "width" D.float
        |> required "x" D.float
        |> required "y" D.float



{- Content -}


cardInfo : List CardInfo
cardInfo =
    [ { title = "FireLyte"
      , content =
            []
      }
    , { title = "IBM Canada"
      , content =
            []
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    , { title = "Nude Solutions"
      , content =
            [ Element.paragraph
                []
                [ Element.text "Despite what the name might allude to, Nude Solutions is a tech company based in Calgary, Alberta. It's primary business is a software platform for insurance companies, brokers and customers. I have been working here for the past year and a half as a software developer, which has provided a lot of interesting opportunities. For example, I was recently able" ]
            ]
      }
    ]



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
