-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Tooltip
  ( -- ** Component
    tooltip_
  ) where
-----------------------------------------------------------------------------
import           Miso
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
tooltip_ :: Component parent model action
tooltip_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ =
  button_
  [ class_ "btn-outline"
  , textProp "data-tooltip" "Default tooltip"
  ]
  [ "Default"
  ]

