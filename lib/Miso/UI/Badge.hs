-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Badge
  ( -- ** Component
    badge_
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
badge_ :: Component parent model action
badge_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = H.span_ [ P.class_ "badge" ][ "Badge" ]
-----------------------------------------------------------------------------
