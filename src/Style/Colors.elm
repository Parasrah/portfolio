module Style.Colors exposing (..)

import Element exposing (Color, rgb255, rgba255)


primary : Color
primary =
    rgb255 46 125 50


primaryLight : Color
primaryLight =
    rgb255 96 173 94


primaryDark : Color
primaryDark =
    rgb255 0 80 5


secondary : Color
secondary =
    rgb255 46 125 89


secondaryLight : Color
secondaryLight =
    rgb255 95 173 134


secondaryDark : Color
secondaryDark =
    rgb255 0 80 48


background : Color
background =
    rgb255 32 33 36


dp00 : Color
dp00 =
    rgba255 255 255 255 0.00

dp01 : Color
dp01 =
    rgba255 255 255 255 0.05


dp02 : Color
dp02 =
    rgba255 255 255 255 0.07


dp03 : Color
dp03 =
    rgba255 255 255 255 0.08


dp04 : Color
dp04 =
    rgba255 255 255 255 0.09


dp06 : Color
dp06 =
    rgba255 255 255 255 0.11


dp08 : Color
dp08 =
    rgba255 255 255 255 0.12


primaryFont : Color
primaryFont =
    rgba255 255 255 255 0.87

secondaryFont : Color
secondaryFont =
    rgba255 255 255 255 0.60
