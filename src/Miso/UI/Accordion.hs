-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Accordion
  ( -- ** Component
    accordion_
    -- ** Types
  , Accordion (..)
  ) where
-----------------------------------------------------------------------------
import Miso
-----------------------------------------------------------------------------
data Accordion
  = Accordion
  { _accordionHeader :: MisoString
    -- ^ Header of the accordion <h2>
  , _accordionSection :: MisoString
    -- ^ Sub section of the accordion <section>
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
accordion_ :: Binding parent Accordion -> Component parent Accordion ()
accordion_ binding = accordionComponent { bindings = [binding] }
-----------------------------------------------------------------------------
accordionComponent :: Component parent Accordion ()
accordionComponent = undefined
-----------------------------------------------------------------------------
