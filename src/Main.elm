port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (Error(..))
import Browser.Events
import Browser.Navigation exposing (Key)
import Element exposing (Attribute, Color, Device, DeviceClass(..), Element, fill, layout, px, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
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
    , cardMetadata : List CardMetadata
    }


type alias Flags =
    { dimensions : Dimensions
    }



-- will be stored in model


type alias CardMetadata =
    { shown : Bool
    , top : Maybe Int
    }



-- info used to render card


type alias CardInfo =
    { title : String
    , content : Device -> List (Element Msg)
    , tags : List String
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
    | ReadMore Int



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

                cardMetadata =
                    List.map
                        (\c -> CardMetadata True Nothing)
                        cardInfo
            in
            ( Page
                { key = key
                , device = device
                , dimensions = flags.dimensions
                , cardMetadata = cardMetadata
                }
            , fetchCardLocations cardMetadata
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
                        [ fetchCardLocations page.cardMetadata
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
                                cardMetadata =
                                    List.map2
                                        (\card location ->
                                            { card | top = Just <| ceiling location.element.y }
                                        )
                                        page.cardMetadata
                                        locations
                            in
                            ( Page { page | cardMetadata = cardMetadata }, Cmd.none )

                        Err err ->
                            ( DomError err, Cmd.none )

                ReadMore index ->
                    ( model, fetchCardLocations page.cardMetadata )

        _ ->
            ( model, Cmd.none )


-- TODO: should I delay this slightly to ensure rendering happens first?
fetchCardLocations : List cards -> Cmd Msg
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
        case model of
            JsonError err ->
                [ layout
                    ([ Element.width fill
                     , Background.color Style.Colors.background
                     , Font.color Style.Colors.primaryFont
                     ]
                        ++ Style.Fonts.regular
                    )
                    (Element.el
                        [ Font.color Style.Colors.primaryFont ]
                        (Element.text "failed to parse json")
                    )
                ]

            DomError err ->
                [ layout
                    ([ Element.width fill
                     , Background.color Style.Colors.background
                     , Font.color Style.Colors.primaryFont
                     ]
                        ++ Style.Fonts.regular
                    )
                    (Element.el
                        [ Font.color Style.Colors.primaryFont ]
                        (Element.text "failed to find cards")
                    )
                ]

            Page page ->
                [ layout
                    ([ Element.width fill
                     , Background.color Style.Colors.background
                     , Font.color Style.Colors.primaryFont
                     , Element.inFront <| viewHeader page
                     ]
                        ++ Style.Fonts.regular
                    )
                    (viewPage page)
                ]
    }


viewPage : IPage -> Element Msg
viewPage page =
    Element.column
        [ Element.paddingEach
            { top = headerHeight page.device.class + 20
            , bottom = 0
            , left = 0
            , right = 0
            }
        ]
        [ viewTimeline page
        ]


viewTimeline : IPage -> Element Msg
viewTimeline page =
    Element.column
        [ Element.height fill
        , Element.width fill
        , let
            paddingX =
                case page.device.class of
                    Phone ->
                        30

                    _ ->
                        50
          in
          Element.paddingEach
            { top = 0
            , left = paddingX
            , right = paddingX
            , bottom = 100
            }
        , Element.spacing <|
            case page.device.class of
                Phone ->
                    50

                Tablet ->
                    70

                Desktop ->
                    50

                BigDesktop ->
                    100
        , Region.mainContent
        ]
        (List.map2 (\metadata info -> ( info, metadata )) page.cardMetadata cardInfo
            |> List.indexedMap
                (\i ( info, metadata ) ->
                    let
                        content =
                            viewCard
                                [ cardAttr i
                                , hidden <| not metadata.shown
                                ]
                                page.device
                                info

                        even =
                            modBy 2 i == 0

                        rowElements =
                            case page.device.class of
                                Phone ->
                                    [ Element.el
                                        [ Element.width fill
                                        ]
                                        content
                                    ]

                                Tablet ->
                                    [ Element.el
                                        [ Element.width <| Element.fillPortion 6
                                        ]
                                        content
                                    , Element.el
                                        [ Element.width <| Element.fillPortion 4 ]
                                        Element.none
                                    ]

                                Desktop ->
                                    [ Element.el
                                        [ Element.width <| Element.fillPortion 6
                                        ]
                                        content
                                    , Element.el
                                        [ Element.width <| Element.fillPortion 4 ]
                                        Element.none
                                    ]

                                BigDesktop ->
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
                        [ Element.width fill
                        ]
                        (if even then
                            rowElements

                         else
                            List.reverse rowElements
                        )
                )
        )



-- TODO: want to have tags on each card
-- TODO: make card slightly larger when hovered (animate)
-- TODO: render cards when they are scrolled 30% into page
-- TODO: if there isn't 30% of page left, render when you can


viewCard : List (Attribute Msg) -> Device -> CardInfo -> Element Msg
viewCard attrs device info =
    let
        rounded =
            5

        headerFooterHeight =
            case device.class of
                Phone ->
                    50

                BigDesktop ->
                    70

                _ ->
                    60

        headerFontSize =
            case device.class of
                Phone ->
                    20

                Tablet ->
                    20

                Desktop ->
                    20

                BigDesktop ->
                    25
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
        ([ Element.el
            [ Background.color Style.Colors.dp01
            , Element.width fill
            , Element.height <| px headerFooterHeight
            , Border.roundEach { topLeft = rounded, topRight = rounded, bottomLeft = 0, bottomRight = 0 }
            ]
            (Element.el
                [ Element.centerY
                , Element.paddingXY 30 0
                , Font.size headerFontSize
                , Region.heading 2
                ]
                (Element.text info.title)
            )
         , Element.textColumn
            [ Element.padding <|
                case device.class of
                    Phone ->
                        20

                    _ ->
                        15
            , Font.size <|
                case device.class of
                    Phone ->
                        15

                    Tablet ->
                        18

                    Desktop ->
                        18

                    BigDesktop ->
                        22
            , Element.width fill
            , Element.spacing 20
            ]
            (info.content device)
         ]
            ++ (if List.isEmpty info.tags then
                    []

                else
                    [ Element.row
                        [ Element.height <| px headerFooterHeight
                        , Element.width fill
                        , Element.spacing <|
                            case device.class of
                                Phone ->
                                    5

                                _ ->
                                    10
                        , let
                            paddingX =
                                case device.class of
                                    Phone ->
                                        10

                                    _ ->
                                        20
                          in
                          Element.paddingXY 20 0
                        ]
                        (List.map
                            (\tag ->
                                Element.el
                                    [ Font.size <|
                                        case device.class of
                                            Phone ->
                                                12

                                            _ ->
                                                15
                                    , Border.solid
                                    , Border.width 1
                                    , Border.rounded 15
                                    , Border.color Style.Colors.secondaryFont
                                    , Font.color Style.Colors.secondaryFont
                                    , Element.padding 7
                                    ]
                                    (Element.text tag)
                            )
                            info.tags
                        )
                    ]
               )
        )


headerHeight : DeviceClass -> Int
headerHeight class =
    75


viewHeader : IPage -> Element Msg
viewHeader page =
    let
        ( spacing, titleFont ) =
            case page.device.class of
                Phone ->
                    ( 15, 25 )

                _ ->
                    ( 35, 30 )
    in
    Element.row
        ([ Element.height <| px <| headerHeight page.device.class
         , Element.width fill
         , Region.navigation
         , Background.color Style.Colors.background
         ]
            ++ Style.Fonts.header
        )
        [ Element.link
            [ Region.heading 1
            , Element.moveRight 30
            , Font.size titleFont
            ]
            { url = "/"
            , label = Element.text "Brad Pfannmuller"
            }
        , Element.row
            [ Element.height fill
            , Element.alignRight
            , Element.moveLeft 40
            , Element.spacing spacing
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


hidden : Bool -> Attribute Msg
hidden bool =
    Element.htmlAttribute <| Html.Attributes.hidden bool



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


scaleImage : Int -> Int -> Int -> List (Attribute msg)
scaleImage oWidth oHeight width =
    let
        ratio =
            toFloat oWidth / toFloat oHeight

        height =
            floor <| toFloat width / ratio
    in
    [ Element.height <| px height
    , Element.width <| px width
    ]


cardInfo : List CardInfo
cardInfo =
    [ CardInfo "Nude Solutions"
        (\device ->
            [ Element.paragraph
                []
                [ Element.newTabLink
                    [ Font.underline ]
                    { url = "https://www.nudesolutions.com/"
                    , label = Element.text "Nude Solutions"
                    }
                , Element.text <|
                    " is a software company focused on building a modern software platform to improve"
                        ++ " the insurance industry for all participants, be it insurance companies, brokers or the customers"
                        ++ " themselves"
                ]
            , Element.paragraph
                []
                [ Element.text <|
                    "I have been working at Nude Solutions since 2019, where I've been working on a new"
                        ++ " project that would simplify the process of offering insurance products on our platform."
                        ++ " As part of this I spear-headed development on an in-house interpreter called SimpleCode"
                        ++ " so our customers can convey complicated business logic without needing to learn a full"
                        ++ " fledged programming language."
                ]
            , Element.paragraph
                []
                [ Element.text <|
                    "In addition, I've been very involved with the move to .NET Core, Linux support, Docker adoption and the"
                        ++ " use of Azure App Services by the company. I also recently gave a company-wide presentation"
                        ++ " on advanced techniques in Typescript."
                ]
            ]
        )
        [ ".NET"
        , "React"
        , "Typescript"
        , "Azure"
        ]
    , CardInfo "FireLyte"
        (\device ->
            [ Element.paragraph
                []
                [ Element.text <|
                    "2020 was the year that saw me launch my own company, CodeGolem Ltd."
                        ++ " One of the reasons behind this decision was to start working on my own products, the first of which is FireLyte."
                ]
            , Element.image
                (let
                    scale =
                        scaleImage 250 354

                    attrs =
                        case device.class of
                            Phone ->
                                scale 120

                            _ ->
                                scale 200
                 in
                 [ Element.alignRight ] ++ attrs
                )
                { src = "/camping.jpg"
                , description = "Stock photo of camping"
                }
            , Element.paragraph
                []
                [ Element.text <|
                    " FireLyte is a multi-tenant software platform targeting the camping industry. Like many Albertans, I love camping,"
                        ++ " and couldn't help feeling technology would dramatically improve the experience, both for those already working in"
                        ++ " the camping industry, and customers trying to find a place to camp. What I have so far is built using Elixir/Phoenix, Elm and"
                        ++ " Nix. If you're interested you can take a look at the source "
                , Element.newTabLink
                    [ Font.underline ]
                    { url = "https://github.com/code-golem/campground"
                    , label = Element.text "here"
                    }
                , Element.text "!"
                ]
            ]
        )
        [ "Nix"
        , "Elm"
        , "Elixir"
        ]
    , CardInfo "Private Internet Access"
        (\device ->
            [ Element.paragraph
                []
                [ Element.text "Finish PIA pls" ]
            ]
        )
        [ "Privacy"
        , "React"
        , "WebExtensions"
        ]
    , CardInfo "Ease"
        (\device ->
            [ Element.paragraph
                []
                [ Element.text "Finish Ease pls" ]
            ]
        )
        [ "P2P"
        , "React"
        , "Redux"
        ]
    , CardInfo "IBM Canada"
        (\device ->
            [ Element.paragraph
                []
                [ Element.text "Finish IBM pls" ]
            ]
        )
        [ "DevOps"
        , "Internship"
        , "Selenium"
        ]
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
