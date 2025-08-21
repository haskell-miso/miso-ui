-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Switch
  ( -- ** Component
    switch_
  ) where
-----------------------------------------------------------------------------
import           Miso
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
switch_ :: Component parent model action
switch_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = label_
  [ class_ "label" ]
  [ input_
    [ type_ "checkbox"
    , name_ "switch"
    , role_ "switch"
    , class_ "input"
    ]
  , "Airplane Mode"
  ]
-----------------------------------------------------------------------------

