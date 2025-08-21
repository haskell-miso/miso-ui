-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.RadioGroup
  ( -- ** Component
    radioGroup_
  ) where
-----------------------------------------------------------------------------
import           Miso
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
radioGroup_ :: Component parent model action
radioGroup_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = fieldset_
    [ class_ "grid gap-3" ]
    [ label_
        [ class_ "label" ]
        [ input_
            [ type_ "radio"
            , name_ "radio-group"
            , value_ "default"
            , class_ "input"
            ]
        , "Default"
        ]
    , label_
        [ class_ "label" ]
        [ input_
            [ type_ "radio"
            , name_ "radio-group"
            , value_ "comfortable"
            , class_ "input"
            , checked_ True
            ]
        , "Comfortable"
        ]
    , label_
        [ class_ "label" ]
        [ input_
            [ type_ "radio"
            , name_ "radio-group"
            , value_ "compact"
            , class_ "input"
            ]
        , "Compact"
        ]
    ]
-----------------------------------------------------------------------------
