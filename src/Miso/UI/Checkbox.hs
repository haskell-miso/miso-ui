-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Checkbox
  ( -- ** Component
    checkbox_
  ) where
-----------------------------------------------------------------------------
import           Miso
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
checkbox_ :: Component parent model action
checkbox_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ =
  label_
  [ class_ "label gap-3" ]
  [ input_ 
    [ type_ "checkbox" 
    , class_ "input" 
    ] 
  , "Accept terms and conditions" 
  ] 
-----------------------------------------------------------------------------
