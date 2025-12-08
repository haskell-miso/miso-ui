-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Avatar
  ( -- ** Views
    avatar_
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
avatar_
  :: [Attribute action]
  -> View model action
avatar_ attrs =
  H.img_ (attrs ++ [ P.class_ "size-8 shrink-0 object-cover rounded-full" ])
