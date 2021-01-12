port module Main exposing (main)

import Animator exposing (Animator)
import Animator.Inline
import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (Error(..))
import Browser.Events
import Browser.Navigation exposing (Key)
import Color
import Element exposing (Attribute, Color, Device, DeviceClass(..), Element, fill, layout, px, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html.Attributes
import Json.Decode as D exposing (Decoder, Value, int)
import Json.Decode.Pipeline exposing (required)
import List.Extra
import Style.Colors
import Style.Fonts
import Task
import Time
import Url exposing (Url)



{- Config -}


type alias Config =
    { pagePaddingTop : Int
    , timelinePaddingX : Int
    , timelineSpacing : Int
    , headerFooterHeight : Int
    , cardHeaderFontSize : Int
    , cardFontSize : Int
    , tagSpacing : Int
    , tagBorderWidth : Int
    , cardContentPadding : Int
    , iconSize : Int
    , cardHeaderPaddingX : Int
    , headerFontSize : Int
    , headerHeight : Int
    , headerIconSpacing : Int
    , summaryFillerPortion : Int
    , bioImageSize : Int
    }


config : Device -> Config
config device =
    case device.class of
        Phone ->
            { pagePaddingTop = 30
            , timelinePaddingX = 30
            , timelineSpacing = 50
            , headerFooterHeight = 50
            , cardHeaderFontSize = 18
            , cardFontSize = 15
            , tagSpacing = 12
            , tagBorderWidth = 1
            , cardContentPadding = 20
            , iconSize = 20
            , cardHeaderPaddingX = 20
            , headerFontSize = 24
            , headerHeight = 75
            , headerIconSpacing = 15
            , summaryFillerPortion = 0
            , bioImageSize = 120
            }

        Tablet ->
            { pagePaddingTop = 30
            , timelinePaddingX = 50
            , timelineSpacing = 70
            , headerFooterHeight = 60
            , cardHeaderFontSize = 20
            , cardFontSize = 18
            , tagSpacing = 15
            , tagBorderWidth = 1
            , cardContentPadding = 25
            , iconSize = 24
            , cardHeaderPaddingX = 30
            , headerFontSize = 30
            , headerHeight = 75
            , headerIconSpacing = 30
            , summaryFillerPortion = 1
            , bioImageSize = 300
            }

        Desktop ->
            { pagePaddingTop = 40
            , timelinePaddingX = 50
            , timelineSpacing = 75
            , headerFooterHeight = 60
            , cardHeaderFontSize = 20
            , cardFontSize = 20
            , tagSpacing = 15
            , tagBorderWidth = 2
            , cardContentPadding = 25
            , iconSize = 24
            , cardHeaderPaddingX = 30
            , headerFontSize = 30
            , headerHeight = 75
            , headerIconSpacing = 30
            , summaryFillerPortion = 1
            , bioImageSize = 400
            }

        BigDesktop ->
            { pagePaddingTop = 80
            , timelinePaddingX = 60
            , timelineSpacing = 100
            , headerFooterHeight = 70
            , cardHeaderFontSize = 25
            , cardFontSize = 22
            , tagSpacing = 15
            , tagBorderWidth = 2
            , cardContentPadding = 25
            , iconSize = 24
            , cardHeaderPaddingX = 30
            , headerFontSize = 30
            , headerHeight = 75
            , headerIconSpacing = 30
            , summaryFillerPortion = 2
            , bioImageSize = 350
            }



{- Model -}


type Model
    = Page IPage
    | JsonError D.Error
    | DomError Browser.Dom.Error


type CardState
    = AlwaysShown
    | Shown
    | Hidden
    | Default


type alias Cards =
    Animator.Timeline (List Card)


type alias IPage =
    { key : Key
    , dimensions : Dimensions
    , device : Device
    , cards : Cards
    , summaryHovered : Animator.Timeline Bool
    }


type alias Flags =
    { dimensions : Dimensions
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


type alias Card =
    { shown : CardState
    , hovered : Bool
    , render : Device -> CardInfo
    }


type alias ImageInfo =
    { src : String
    , description : String
    , ratio : Float
    }


type alias CardInfo =
    { title : String
    , content : List (Element Msg)
    , tags : List String
    , desktopImage : ImageInfo
    }



{- Animations -}


pageAnimator : Animator IPage
pageAnimator =
    Animator.animator
        |> Animator.watching
            .cards
            (\newCards model -> { model | cards = newCards })
        |> Animator.watching
            .summaryHovered
            (\newSummaryHovered model -> { model | summaryHovered = newSummaryHovered })



{- Message -}


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest
    | OnDimensions Dimensions
    | OnResize
    | OnScroll
    | OnCardLocations (Result Browser.Dom.Error (List Browser.Dom.Element))
      -- TODO: allow collapsing the cards on mobile
    | ReadMore Int
    | Tick Time.Posix
    | CardHover Int Bool
    | SummaryHover Bool



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
                        (\c -> Card Default False c)
                        cardInfo

                animatedCards =
                    Animator.init cardMetadata
            in
            ( Page
                { key = key
                , device = device
                , dimensions = flags.dimensions
                , cards = animatedCards
                , summaryHovered = Animator.init False
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
                        [ fetchCardLocations <| Animator.current page.cards
                        , Task.perform OnDimensions Browser.Dom.getViewport
                        ]
                    )

                -- only need to update dimensions, top of browser elements haven't
                -- moved
                OnScroll ->
                    ( model
                    , Cmd.batch
                        [ Task.perform OnDimensions Browser.Dom.getViewport
                        , fetchCardLocations <| Animator.current page.cards
                        ]
                    )

                OnCardLocations res ->
                    case res of
                        Ok locations ->
                            let
                                updateShown isShown card =
                                    { card | shown = isShown }

                                updateCard card location =
                                    let
                                        isElementOnPage l =
                                            l.element.y < l.viewport.height + l.viewport.y
                                    in
                                    case card.shown of
                                        Default ->
                                            if isElementOnPage location then
                                                { card | shown = AlwaysShown }

                                            else
                                                { card | shown = Hidden }

                                        AlwaysShown ->
                                            card

                                        Shown ->
                                            card

                                        Hidden ->
                                            let
                                                factor =
                                                    0.7

                                                bottomOutFactor =
                                                    0.1

                                                threshold =
                                                    location.viewport.height * factor + location.viewport.y

                                                distanceToThreshold =
                                                    location.element.y - threshold
                                            in
                                            if distanceToThreshold < 0 then
                                                card
                                                    |> updateShown Shown

                                            else if location.scene.height - (location.viewport.y + location.viewport.height) < location.viewport.height * bottomOutFactor then
                                                card
                                                    |> updateShown Shown

                                            else
                                                card

                                cards =
                                    List.map2 updateCard (Animator.current page.cards) locations
                            in
                            ( Page
                                { page
                                    | cards = Animator.go Animator.slowly cards page.cards
                                }
                            , Cmd.none
                            )

                        Err err ->
                            ( DomError err, Cmd.none )

                ReadMore index ->
                    ( model, fetchCardLocations <| Animator.current page.cards )

                Tick newTime ->
                    ( Page <| Animator.update newTime pageAnimator page, Cmd.none )

                CardHover i isHovered ->
                    let
                        cardMetadata =
                            Animator.current page.cards
                    in
                    case List.Extra.getAt i cardMetadata of
                        Just card ->
                            let
                                newCard =
                                    { card | hovered = isHovered }

                                newCards =
                                    List.Extra.setAt i newCard cardMetadata
                            in
                            ( Page { page | cards = Animator.go Animator.slowly newCards page.cards }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                SummaryHover isHovered ->
                    ( Page { page | summaryHovered = Animator.go Animator.slowly isHovered page.summaryHovered }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
subscriptions model =
    case model of
        Page page ->
            Sub.batch
                [ Browser.Events.onResize (\_ _ -> OnResize)
                , onScroll (\_ -> OnScroll)
                , Animator.toSubscription Tick page pageAnimator
                ]

        _ ->
            Sub.none



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
                     , Element.clipX
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
                    ([ Element.htmlAttribute <| Html.Attributes.style "overflow-x" "hidden"
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
        [ let
            paddingX =
                config page.device |> .timelinePaddingX
          in
          Element.paddingEach
            { top =
                (config page.device |> .headerHeight)
                    + (config page.device |> .pagePaddingTop)
            , bottom = 80
            , left = paddingX
            , right = paddingX
            }
        , Element.spacing (config page.device |> .timelineSpacing)
        , Region.mainContent
        ]
        -- [ viewSummary page
        [ viewTimeline page
        ]


viewSummary : IPage -> Element Msg
viewSummary page =
    let
        rounded =
            5

        { device } =
            page

        headerFooterHeight =
            config device |> .headerFooterHeight

        headerFontSize =
            config device |> .cardHeaderFontSize

        cardFontSize =
            config device |> .cardFontSize

        bioImageSize =
            config device |> .bioImageSize

        summaryFillerPortion =
            config device |> .summaryFillerPortion

        p1 =
            Element.paragraph
                [ Font.size cardFontSize ]
                [ Element.text <|
                    "Hi! My name is Brad Pfannmuller, and I built this website to tell"
                        ++ " you a little bit about myself! I live in a small apartment"
                        ++ " with my girlfriend in Calgary, Alberta."
                ]

        p2 =
            Element.paragraph
                [ Font.size cardFontSize ]
                [ Element.text <|
                    "I was fortunate to grow up so close to the Rocky Mountains, and spent"
                        ++ " a lot of my childhood camping. As an adult I still enjoy camping,"
                        ++ " and also enjoy hiking, video games and coding. I hope you learn"
                        ++ " "
                ]

        profile =
            Element.image
                [ Border.rounded (bioImageSize // 2)
                , Element.clip
                , Element.width <| px bioImageSize
                , Element.height <| px bioImageSize
                , Element.alignLeft
                ]
                { src = "/brad.jpg"
                , description = "A picture of me!"
                }

        mobileHeading () =
            Element.el
                [ Background.color Style.Colors.dp01
                , Element.width fill
                , Element.height <| px headerFooterHeight
                , Border.roundEach { topLeft = rounded, topRight = rounded, bottomLeft = 0, bottomRight = 0 }
                ]
                (Element.el
                    [ Element.centerY
                    , Element.paddingXY (config device |> .cardHeaderPaddingX) 0
                    , Font.size headerFontSize
                    , Region.heading 2
                    ]
                    (Element.text "Bio")
                )
    in
    Element.row
        [ Element.width fill ]
        [ Element.el [ Element.width <| Element.fillPortion summaryFillerPortion ] Element.none
        , Element.column
            [ Background.color Style.Colors.dp01
            , Element.width <| Element.fillPortion 5
            , Font.color <| Style.Colors.primaryFont
            , Element.alignLeft
            , Element.centerY
            , Border.rounded rounded
            , Border.solid
            , Events.onMouseEnter <| SummaryHover True
            , Events.onMouseLeave <| SummaryHover False
            , Animator.Inline.transform
                { position = { x = 0, y = 0 }
                , rotate = 0
                , scale =
                    Animator.move page.summaryHovered <|
                        \state ->
                            if state then
                                Animator.at 1.04

                            else
                                Animator.at 1
                }
                |> Element.htmlAttribute
            , Background.color <|
                Element.fromRgb <|
                    Color.toRgba <|
                        Animator.color page.summaryHovered <|
                            \state ->
                                if state then
                                    Style.Colors.dp04Color

                                else
                                    Style.Colors.dp01Color
            ]
            (if isMobile page.device then
                [ Element.column
                    [ Element.width fill
                    , Element.padding 20
                    , Element.spacing 20
                    ]
                    [ Element.row
                        [ Element.spacing 20 ]
                        [ profile
                        , p1
                        ]
                    , p2
                    ]
                ]

             else
                [ Element.textColumn
                    [ Element.padding (config device |> .cardContentPadding)
                    , Font.size cardFontSize
                    , Element.width fill
                    , Element.spacing 20
                    ]
                    [ Element.textColumn
                        []
                        [ profile
                        , p1
                        , p2
                        ]
                    ]
                ]
            )
        , Element.el [ Element.width <| Element.fillPortion summaryFillerPortion ] Element.none
        ]


viewTimeline : IPage -> Element Msg
viewTimeline page =
    Element.column
        [ Element.height fill
        , Element.width fill
        , Element.spacing (config page.device |> .timelineSpacing)
        ]
        (Animator.current page.cards
            |> List.indexedMap
                (\i card ->
                    let
                        info =
                            card.render page.device

                        content =
                            viewCard
                                page.device
                                page.dimensions
                                page.cards
                                i

                        even =
                            modBy 2 i == 0

                        rowElements =
                            let
                                { desktopImage } =
                                    info
                            in
                            case page.device.class of
                                Phone ->
                                    [ Element.el
                                        [ Element.width fill
                                        ]
                                        content
                                    ]

                                Tablet ->
                                    let
                                        ( imageWidth, imageHeight ) =
                                            calculateImageDimensions ( 200, 200 ) desktopImage.ratio
                                    in
                                    [ Element.el
                                        [ Element.width <| Element.fillPortion 6
                                        ]
                                        content
                                    , Element.el
                                        [ Element.width <| Element.fillPortion 4
                                        , Element.centerX
                                        , Element.centerY
                                        ]
                                        (Element.image
                                            ([ Element.height <| px imageHeight
                                             , Element.width <| px imageWidth
                                             , Element.centerX
                                             ]
                                                ++ (case card.shown of
                                                        Shown ->
                                                            if modBy 2 i == 0 then
                                                                [ fadeInRight, delayAnimation ]

                                                            else
                                                                [ fadeInLeft, delayAnimation ]

                                                        _ ->
                                                            []
                                                   )
                                            )
                                            { src = desktopImage.src
                                            , description = desktopImage.description
                                            }
                                        )
                                    ]

                                Desktop ->
                                    let
                                        ( imageWidth, imageHeight ) =
                                            calculateImageDimensions ( 400, 300 ) desktopImage.ratio
                                    in
                                    [ Element.el
                                        [ Element.width <| Element.fillPortion 6
                                        ]
                                        content
                                    , Element.el
                                        [ Element.width <| Element.fillPortion 4
                                        , Element.centerX
                                        , Element.centerY
                                        ]
                                        (Element.image
                                            ([ Element.height <| px imageHeight
                                             , Element.width <| px imageWidth
                                             , Element.centerX
                                             ]
                                                ++ (case card.shown of
                                                        Shown ->
                                                            if modBy 2 i == 0 then
                                                                [ fadeInRight, delayAnimation ]

                                                            else
                                                                [ fadeInLeft, delayAnimation ]

                                                        _ ->
                                                            []
                                                   )
                                            )
                                            { src = desktopImage.src
                                            , description = desktopImage.description
                                            }
                                        )
                                    ]

                                BigDesktop ->
                                    let
                                        ( imageWidth, imageHeight ) =
                                            calculateImageDimensions ( 500, 300 ) desktopImage.ratio
                                    in
                                    [ Element.el
                                        [ Element.width <| Element.fillPortion 6
                                        ]
                                        content
                                    , Element.el
                                        [ Element.width <| Element.fillPortion 4
                                        ]
                                        (Element.image
                                            ([ Element.height <| px imageHeight
                                             , Element.width <| px imageWidth
                                             , Element.centerX
                                             ]
                                                ++ (case card.shown of
                                                        Shown ->
                                                            if modBy 2 i == 0 then
                                                                [ fadeInRight, delayAnimation ]

                                                            else
                                                                [ fadeInLeft, delayAnimation ]

                                                        _ ->
                                                            []
                                                   )
                                            )
                                            { src = desktopImage.src
                                            , description = desktopImage.description
                                            }
                                        )
                                    ]
                    in
                    Element.row
                        [ Element.width fill
                        , card
                            |> .shown
                            |> isVisible
                            |> not
                            |> visibility
                        ]
                        (if even then
                            rowElements

                         else
                            List.reverse rowElements
                        )
                )
        )


calculateImageDimensions : ( Int, Int ) -> Float -> ( Int, Int )
calculateImageDimensions ( maxWidth, maxHeight ) ratio =
    -- ratio = width / height
    -- ratio * height = width / height * height
    -- ratio * height = width
    -- ratio / width = width / height / width
    -- ratio / width = 1 / height
    -- width / ratio = height
    let
        widthWithMaxHeight =
            ratio * toFloat maxHeight
    in
    if widthWithMaxHeight > toFloat maxWidth then
        ( maxWidth, floor (toFloat maxWidth / ratio) )

    else
        ( floor widthWithMaxHeight, maxHeight )


getCard : Int -> List Card -> Card
getCard i cards =
    List.Extra.getAt i cards
        |> Maybe.withDefault
            { shown = Shown
            , hovered = False
            , render =
                \device ->
                    CardInfo "Fallback card"
                        []
                        []
                        { src = ""
                        , description = ""
                        , ratio = 0
                        }
            }


viewCard : Device -> Dimensions -> Cards -> Int -> Element Msg
viewCard device dimensions cards i =
    let
        isEven =
            modBy 2 i == 0

        card =
            getCard i <| Animator.current cards

        info =
            card.render device

        rounded =
            5

        headerFooterHeight =
            config device |> .headerFooterHeight

        headerFontSize =
            config device |> .cardHeaderFontSize

        cardFontSize =
            config device |> .cardFontSize
    in
    Element.column
        ([ Background.color Style.Colors.dp01
         , Element.width fill
         , Font.color <| Style.Colors.primaryFont
         , Element.alignLeft
         , Element.centerY
         , Border.rounded rounded
         , Border.solid
         , Animator.Inline.transform
            { position = { x = 0, y = 0 }
            , rotate = 0
            , scale =
                Animator.move cards <|
                    \state ->
                        if getCard i state |> .hovered then
                            Animator.at 1.04

                        else
                            Animator.at 1
            }
            |> Element.htmlAttribute
         , Background.color <|
            Element.fromRgb <|
                Color.toRgba <|
                    Animator.color cards <|
                        \state ->
                            if getCard i state |> .hovered then
                                Style.Colors.dp04Color

                            else
                                Style.Colors.dp01Color
         , cardAttr i
         , Events.onMouseEnter <| CardHover i True
         , Events.onMouseLeave <| CardHover i False
         ]
            ++ (case card.shown of
                    Shown ->
                        if isEven then
                            [ fadeInLeft ]

                        else
                            [ fadeInRight ]

                    _ ->
                        []
               )
        )
        ([ Element.el
            [ Background.color Style.Colors.dp01
            , Element.width fill
            , Element.height <| px headerFooterHeight
            , Border.roundEach { topLeft = rounded, topRight = rounded, bottomLeft = 0, bottomRight = 0 }
            ]
            (Element.el
                [ Element.centerY
                , Element.paddingXY (config device |> .cardHeaderPaddingX) 0
                , Font.size headerFontSize
                , Region.heading 2
                ]
                (Element.text info.title)
            )
         , Element.textColumn
            [ Element.padding (config device |> .cardContentPadding)
            , Font.size cardFontSize
            , Element.width fill
            , Element.spacing 20
            ]
            info.content
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
                                    [ Font.size (config device |> .tagSpacing)
                                    , Border.solid
                                    , Border.width (config device |> .tagBorderWidth)
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


isMobile : Device -> Bool
isMobile { class } =
    case class of
        Phone ->
            True

        _ ->
            False


isVisible : CardState -> Bool
isVisible state =
    case state of
        Hidden ->
            False

        AlwaysShown ->
            True

        Default ->
            True

        Shown ->
            True


viewHeader : IPage -> Element Msg
viewHeader page =
    Element.row
        ([ Element.height <| px <| (config page.device |> .headerHeight)
         , Element.width fill
         , Region.navigation
         , Background.color Style.Colors.background
         ]
            ++ Style.Fonts.header
        )
        [ Element.link
            [ Region.heading 1
            , Element.moveRight 30
            , Font.size (config page.device |> .headerFontSize)
            ]
            { url = "/"
            , label = Element.text "Brad Pfannmuller"
            }
        , Element.row
            [ Element.height fill
            , Element.alignRight
            , Element.moveLeft 40
            , Element.spacing (config page.device |> .headerIconSpacing)
            ]
            [ Element.newTabLink
                [ Region.description "My github account"
                ]
                { url = "https://github.com/parasrah"
                , label =
                    Element.image
                        [ Element.alignRight
                        , Element.width <| px (config page.device |> .iconSize)
                        , Element.height <| px (config page.device |> .iconSize)
                        ]
                        { src = "/github.svg"
                        , description = "Github, where I host most of my code"
                        }
                }
            , Element.newTabLink
                [ Region.description "My LinkedIn account"
                ]
                { url = "https://www.linkedin.com/in/brad-pfannmuller/"
                , label =
                    Element.image
                        [ Element.alignRight
                        , Element.width <| px (config page.device |> .iconSize)
                        , Element.height <| px (config page.device |> .iconSize)
                        ]
                        { src = "/linkedin.svg"
                        , description = "LinkedIn, the popular career platform"
                        }
                }
            , Element.link
                [ Region.description "My email"
                ]
                { url = "mailto:jobs@parasrah.com"
                , label =
                    Element.image
                        [ Element.alignRight
                        , Element.width <| px (config page.device |> .iconSize)
                        , Element.height <| px (config page.device |> .iconSize)
                        ]
                        { src = "/mail.svg"
                        , description = "Send me an email!"
                        }
                }
            , Element.newTabLink
                [ Region.description "Source code for my portfolio"
                ]
                { url = "https://github.com/parasrah/portfolio"
                , label =
                    Element.image
                        [ Element.alignRight
                        , Element.width <| px (config page.device |> .iconSize)
                        , Element.height <| px (config page.device |> .iconSize)
                        ]
                        { src = "/code.svg"
                        , description = "Source code for this website"
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


delayAnimation : Attribute Msg
delayAnimation =
    Element.htmlAttribute <| Html.Attributes.class "animate__delay-1s"


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


visibility : Bool -> Attribute Msg
visibility flag =
    if flag then
        Element.htmlAttribute <| Html.Attributes.style "visibility" "hidden"

    else
        Element.htmlAttribute <| Html.Attributes.style "visibility" "visible"



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


cardInfo : List (Device -> CardInfo)
cardInfo =
    [ \device ->
        CardInfo "Nude Solutions"
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
            [ ".NET"
            , "React"
            , "Azure"
            , "Remote"
            ]
            { src = "/nudelogo.svg"
            , description = "Nude Solutions logo"
            , ratio = 773.23 / 252.68
            }
    , \device ->
        CardInfo "FireLyte"
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

                            Tablet ->
                                scale 150

                            _ ->
                                scale 200
                 in
                 [ Element.alignRight
                 , Border.rounded 5
                 , Element.clip
                 ]
                    ++ attrs
                )
                { src = "/camping.jpg"
                , description = "Stock photo of camping"
                }
            , Element.paragraph
                []
                [ Element.text <|
                    " FireLyte is a multi-tenant software platform targeting the camping industry. Like many Albertans, I love camping,"
                        ++ " and couldn't help feeling technology would dramatically improve the experience; both for those already working in"
                        ++ " the camping industry and customers trying to find a place to camp. What I have so far is built using Elixir/Phoenix, Elm and"
                        ++ " Nix. If you're interested you can take a look at the source "
                , Element.newTabLink
                    [ Font.underline
                    , Region.description "Source code of FireLyte"
                    ]
                    { url = "https://github.com/code-golem/campground"
                    , label = Element.text "here"
                    }
                , Element.text "!"
                ]
            ]
            [ "Nix"
            , "Elm"
            , "Elixir"
            , "Multi-Tenant"
            ]
            { src = "/code-golem.svg"
            , description = "Icon used for the CodeGolem github repo"
            , ratio = 1
            }
    , \device ->
        CardInfo "Nix"
            [ Element.paragraph
                []
                [ Element.text <|
                    "Until 2019, I would find myself re-installing whatever distro I happened to be"
                        ++ " using every year, in an attempt to avoid all the inadvertent side-effects I'd"
                        ++ " accidentally incurred during my daily use. I had heard of the nix package"
                        ++ " manager before, even tried it out, but hadn't given it further thought. That is,"
                        ++ " until I discovered NixOS."
                ]
            , Element.paragraph
                []
                [ Element.text <|
                    "I was instantly hooked. I have always enjoyed DevOps; tinkering with CI and CD on my"
                        ++ " personal projects always brought with it a quiet tranquility as the hours passed"
                        ++ " by. NixOS just opened the flood gates. Originally enticed by not having to refresh my"
                        ++ " machines, the idea was quickly reinforced as I discovered I could roll back "
                        ++ " to a previous state "
                , Element.el [ Font.strike ] (Element.text "if")
                , Element.text <|
                    " when I messed something up. The thing is, it didn't stop there. There were constantly new"
                        ++ " things to discover, challenges to overcome and novel features to try out. For months"
                        ++ " I spent my free time exploring the possibilities, trying to get it working"
                        ++ " with Elm and Elixir and just trying to learn more. Finally, I started putting my"
                        ++ " newfound knowledge to work in the form of FireLyte."
                ]
            , Element.paragraph []
                [ Element.text <|
                    "Nix has cost me countless hours, taught me countless lessons and shown me how much I enjoy"
                        ++ " doing this kind of work. And I can't wait to learn more."
                ]
            ]
            [ "DevOps"
            , "Linux"
            , "Nix"
            ]
            { src = "/nix.svg"
            , description = "Nix's Logo"
            , ratio = 535.15 / 464.15
            }
    , \device ->
        CardInfo "Private Internet Access"
            [ Element.paragraph
                []
                [ Element.text <|
                    "Private Internet Access is a remote-first company that provides a VPN"
                        ++ " service to improve the privacy and security of their customers."
                        ++ " I started working here in 2018, and was responsible for working"
                        ++ " on their Chrome & Firefox web extensions."
                ]
            , Element.paragraph
                []
                [ Element.text <|
                    "As someone who cares deeply about privacy, this was somewhat of a dream"
                        ++ " job for me coming out of school. I got to work with some very"
                        ++ " talented individuals and experience working on a global product."
                        ++ " This was my first time working on something so many people already"
                        ++ " relied on day to day, which really forced me to change the way I"
                        ++ " thought about development."
                ]
            , Element.paragraph
                []
                [ Element.text <|
                    "Unfortunately, due to some ethical issues I had with the companies changing vision,"
                        ++ " I ended up resigning after only a year in 2019 and taking the summer to"
                        ++ " work on a blogging platform (currently unfinished) before starting at Nude Solutions"
                ]
            ]
            [ "Privacy"
            , "React"
            , "Web Extensions"
            , "Remote"
            ]
            { src = "/pia.png"
            , description = "Private Internet Access icon"
            , ratio = 1
            }
    , \device ->
        CardInfo "IBM Canada"
            [ Element.paragraph
                []
                [ Element.text <|
                    "In 2016, I had my first industry experience working at IBM Canada on a 12 month long internship. Despite my title being"
                        ++ " \"Responsive Web & Java Developer\", it mostly involved me managing our internal CI servers, and"
                        ++ " writing Selenium UI tests (and lots of meetings)."
                ]
            , Element.paragraph
                []
                [ Element.text <|
                    "More interesting was my participation in the Future Blue team, a small group of interns"
                        ++ " voted into positions to support the other interns that year. After being part of the Future Blue"
                        ++ " Web Team for a few months, I was elected to be one of the two leaders. This involved us running"
                        ++ " weekly sessions for the members to plan and work on projects, planning the yearly Web Hackathon"
                        ++ " and volunteering for events such as \"Take your kids to work\" day."
                ]
            ]
            [ "DevOps"
            , "Internship"
            , "Selenium"
            ]
            { src = "/IBM_logo.svg"
            , description = "IBM's logo"
            , ratio = 1000 / 400
            }
    , \device ->
        CardInfo "Ease"
            [ Element.paragraph
                []
                [ Element.text <|
                    "During my internship at IBM, I began working on a project in my free time. At the time I was doing"
                        ++ " long distance with my girlfriend, and "
                , Element.newTabLink
                    [ Font.underline
                    , Region.description "Ease source code"
                    ]
                    { url = "https://github.com/parasrah/ease"
                    , label = Element.text "Ease"
                    }
                , Element.text <|
                    " was an Electron app that let me stream movies to her with shared controls. I achieved this using"
                        ++ " WebRTC to prevent needing to run anything beyond a small signal server."
                ]
            , Element.paragraph
                []
                [ Element.text <|
                    "Beyond this being my first serious project leading me to learn technologies like React, Webpack"
                        ++ " and just Javascript in general (thank you Kyle Simpson), this was also the project that sparked my love for"
                        ++ " software. Since this point, I've consistently been working on various projects in my free time. Although"
                        ++ " many of them are still unfinished, each and every one has taught me something invaluable along the way."
                ]
            ]
            [ "P2P"
            , "React"
            , "Redux"
            ]
            { src = "/ease.png"
            , description = "Screenshot of Ease video player"
            , ratio = 500 / 226
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
