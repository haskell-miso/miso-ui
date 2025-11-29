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
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
avatar_ :: Component parent model action
avatar_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = H.img_
    [ P.src_ "https://github.com/hunvreus.png"
    , P.alt_ "@hunvreus"
    , P.class_ "size-8 shrink-0 object-cover rounded-full"
    ]
