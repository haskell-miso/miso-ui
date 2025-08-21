-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Popover
  ( -- ** Component
    popover_
  ) where
-----------------------------------------------------------------------------
import           Miso
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
popover_ :: Component parent model action
popover_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = div_
    [ id_ "demo-popover"
    , class_ "popover"
    ]
    [ button_
        [ id_ "demo-popover-trigger"
        , type_ "button"
        , textProp "aria-expanded" "false"
        , textProp "aria-controls" "demo-popover-popover"
        , class_ "btn-outline"
        ][ "Open popover" ]
    , div_
        [ id_ "demo-popover-popover"
        , textProp "data-popover" ""
        , textProp "aria-hidden" "true"
        , class_ "w-80"
        ]
        [ div_
            [ class_ "grid gap-4" ]
            [ header_
                [ class_ "grid gap-1.5" ]
                [ h4_
                    [ class_ "leading-none font-medium" ][ "Dimensions" ]
                , p_
                    [ class_ "text-muted-foreground text-sm" ][ "Set the dimensions for the layer." ]
                ]
            , form
                [ class_ "form grid gap-2" ]
                [ div_
                    [ class_ "grid grid-cols-3 items-center gap-4" ]
                    [ label_
                        [ for_ "demo-popover-width" ][ "Width" ]
                    , input_
                        [ type_ "text"
                        , id_ "demo-popover-width"
                        , value_ "100%"
                        , class_ "col-span-2 h-8"
                        , textProp "autofocus" ""
                        ]
                    , ">"
                    ]
                , div_
                    [ class_ "grid grid-cols-3 items-center gap-4" ]
                    [ label_
                        [ for_ "demo-popover-max-width" ][ "Max. width" ]
                    , input_
                        [ type_ "text"
                        , id_ "demo-popover-max-width"
                        , value_ "300px"
                        , class_ "col-span-2 h-8"
                        ]
                    , ">"
                    ]
                , div_
                    [ class_ "grid grid-cols-3 items-center gap-4" ]
                    [ label_
                        [ for_ "demo-popover-height" ][ "Height" ]
                    , input_
                        [ type_ "text"
                        , id_ "demo-popover-height"
                        , value_ "25px"
                        , class_ "col-span-2 h-8"
                        ]
                    , ">"
                    ]
                , div_
                    [ class_ "grid grid-cols-3 items-center gap-4" ]
                    [ label_
                        [ for_ "demo-popover-max-height" ][ "Max. height" ]
                    , input_
                        [ type_ "text"
                        , id_ "demo-popover-max-height"
                        , value_ "none"
                        , class_ "col-span-2 h-8"
                        ]
                    , ">"
                    ]
                ]
            ]
        ]
    ]

