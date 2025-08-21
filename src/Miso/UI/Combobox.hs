-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Combobox
  ( -- ** Component
    combobox_
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
combobox_ :: Component parent model action
combobox_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = div_
  [ id_ "select-909078"
  , class_ "select"
  ]
  [ button_
    [ type_ "button"
    , class_ "btn-outline justify-between font-normal w-[200px]"
    , id_ "select-909078-trigger"
    , textProp "aria-haspopup" "listbox"
    , textProp "aria-expanded" "false"
    , textProp "aria-controls" "select-909078-listbox"
    ]
        [ span_
            [ class_ "truncate" ][]
        , svg_
            [ xmlns_ "http://www.w3.org/2000/svg"
            , width_ "24"
            , height_ "24"
            , S.viewBox_ "0 0 24 24"
            , S.fill_ "none"
            , S.stroke_ "currentColor"
            , textProp "stroke-width" "2"
            , textProp "stroke-linecap" "round"
            , textProp "stroke-linejoin" "round"
            , class_ "lucide lucide-chevrons-up-down-icon lucide-chevrons-up-down text-muted-foreground opacity-50 shrink-0"
            ][]
        ]
    , div_
        [ id_ "select-909078-popover"
        , textProp "aria-hidden" "true"
        ]
        [ header_ []
            [ svg_
                [ xmlns_ "http://www.w3.org/2000/svg"
                , width_ "24"
                , height_ "24"
                , S.viewBox_ "0 0 24 24"
                , S.fill_ "none"
                , S.stroke_ "currentColor"
                , textProp "stroke-width" "2"
                , textProp "stroke-linecap" "round"
                , textProp "stroke-linejoin" "round"
                , class_ "lucide lucide-search-icon lucide-search"
                ][]
            , input_
                [ type_ "text"
                , value_ ""
                , placeholder_ "Search framework..."
                , autocomplete_ False
                , autocorrect_ False
                , spellcheck_ False
                , aria_ "autocomplete" "list"
                , role_ "combobox"
                , aria_ "expanded" "false"
                , aria_ "controls" "select-909078-listbox"
                , aria_ "labelledby" "select-909078-trigger"
                ]
            ]
        , div_
            [ role_ "listbox"
            , id_ "select-909078-listbox"
            , textProp "aria-orientation" "vertical"
            , textProp "aria-labelledby" "select-909078-trigger"
            , textProp "data-empty" "No framework found."
            ]
            [ div_
                [ role_ "option"
                , textProp "data-value" "Next.js"
                ][ "Next.js" ]
            , div_
                [ role_ "option"
                , textProp "data-value" "SvelteKit"
                ][ "SvelteKit" ]
            , div_
                [ role_ "option"
                , textProp "data-value" "Nuxt.js"
                ][ "Nuxt.js" ]
            , div_
                [ role_ "option"
                , textProp "data-value" "Remix"
                ][ "Remix" ]
            , div_
                [ role_ "option"
                , textProp "data-value" "Astro"
                ][ "Astro" ]
            ]
        ]
    , input_
        [ type_ "hidden"
        , name_ "select-909078-value"
        , value_ ""
        ]
    ]
-----------------------------------------------------------------------------
