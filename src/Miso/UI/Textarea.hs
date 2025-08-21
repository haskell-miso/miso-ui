-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Textarea
  ( -- ** Component
    textarea_
  ) where
-----------------------------------------------------------------------------
import           Miso hiding (textarea_)
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
textarea_ :: Component parent model action
textarea_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ =
  H.textarea_
  [ P.class_ "textarea"
  , P.placeholder_ "Type your message here"
  ]
  []
