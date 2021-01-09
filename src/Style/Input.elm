module Style.Input exposing (common)

import Element exposing (Attribute, Device)
import Element.Font as Font


common : Device -> List (Attribute msg)
common _ =
    [ Font.size 20
    ]
