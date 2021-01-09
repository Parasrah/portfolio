module Style.Button exposing (common)

import Element exposing (Attribute, Device)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Colors as Colors


common : Device -> List (Attribute msg)
common _ =
    [ Background.color Colors.secondary
    , Element.paddingXY 15 10
    , Border.rounded 4
    , Element.mouseOver
        [ Background.color Colors.secondaryLight
        ]
    , Font.color Colors.primaryFont
    ]
