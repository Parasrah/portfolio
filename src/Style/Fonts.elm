module Style.Fonts exposing (..)

import Element exposing (Attribute)
import Element.Font as Font


regular : List (Attribute msg)
regular =
    [ Font.family
        [ Font.typeface "Noto Sans"
        , Font.sansSerif
        ]
    ]


bold : List (Attribute msg)
bold =
    [ Font.family
        [ Font.typeface "Noto Sans"
        , Font.sansSerif
        ]
    , Font.bold
    ]


italic : List (Attribute msg)
italic =
    [ Font.family
        [ Font.typeface "Noto Sans"
        , Font.sansSerif
        ]
    , Font.italic
    ]


boldItalic : List (Attribute msg)
boldItalic =
    [ Font.family
        [ Font.typeface "Noto Sans"
        , Font.sansSerif
        ]
    , Font.italic
    , Font.bold
    ]


header : List (Attribute msg)
header =
    [ Font.family
        [ Font.typeface "Roboto"
        , Font.sansSerif
        ]
    , Font.medium
    ]
