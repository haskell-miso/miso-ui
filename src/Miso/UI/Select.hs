-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Select
  ( -- ** Component
    select_
  ) where
-----------------------------------------------------------------------------
import           Miso hiding (select_)
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
select_ :: Component parent model action
select_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = H.select_
  [ class_ "select w-[180px]" ]
  [ optgroup_
      [ textProp "label" "Fruits"
      ]
      [ option_ []  [ "Apple" ]
      , option_ []  [ "Banana" ]
      , option_ []  [ "Blueberry" ]
      , option_ []  [ "Grapes" ]
      , option_ []  [ "Pineapple" ]
      ]
  ]

