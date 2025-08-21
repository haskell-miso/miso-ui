-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Avatar
  ( -- ** Component
    avatar_
  ) where
-----------------------------------------------------------------------------
import           Miso
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
avatar_ :: Component parent model action
avatar_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = img_
    [ src_ "https://github.com/hunvreus.png"
    , alt_ "@hunvreus"
    , class_ "size-8 shrink-0 object-cover rounded-full"
    ]
