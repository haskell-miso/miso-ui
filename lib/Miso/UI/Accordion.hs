-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultilineStrings           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Accordion
  ( -- ** Views
    accordion_
  , accordionSection_
  , accordionHeader_
  , accordionBody_
  -- ** Sample
  , accordionSample
  , accordionCodeSample
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html          as H
import qualified Miso.Html.Property as P
import qualified Miso.Svg           as S
import qualified Miso.Svg.Property  as SP
-----------------------------------------------------------------------------
accordion_
  :: [ Attribute a ]
  -> [ View m a ]
  -> View m a
accordion_ attrs kids =
  optionalAttrs
    H.section_
    attrs
    True
    [ P.class_ "accordion"
    ]
    kids
-----------------------------------------------------------------------------
accordionSection_
  :: [ Attribute a ]
  -> [ View m a ]
  -> View m a
accordionSection_ attrs kids =
  optionalAttrs
    H.details_
    attrs
    True
    [ P.classes_
      [ "group"
      , "border-b"
      , "last:border-b-0"
      ]
    ]
    kids
-----------------------------------------------------------------------------
accordionHeader_
  :: [ Attribute a ]
  -> [ View m a ]
  -> View m a
accordionHeader_ attrs kids = optionalAttrs
  H.summary_
  attrs
  True
  [ P.className
      "w-full focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] transition-all outline-none rounded-md"
  ]
  [ H.h2_
    [ P.className "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline"
    ]
    kids
  , H.svg_
      [ P.classes_
          [ "text-muted-foreground"
          , "pointer-events-none"
          , "size-4"
          , "shrink-0"
          , "translate-y-0.5"
          , "transition-transform"
          , "duration-200"
          , "group-open:rotate-180"
          ]
      , SP.strokeLinejoin_ "round"
      , SP.strokeLinecap_ "round"
      , SP.strokeWidth_ "2"
      , SP.stroke_ "currentColor"
      , SP.fill_ "none"
      , SP.viewBox_ "0 0 24 24"
      , P.height_ "24"
      , P.width_ "24"
      , P.xmlns_ "http://www.w3.org/2000/svg"
      ]
    [ S.path_
      [ SP.d_ "m6 9 6 6 6-6"
      ]
    ]
  ]
-----------------------------------------------------------------------------
accordionBody_
  :: [ Attribute a ]
  -> [ View m a ]
  -> View m a
accordionBody_ attrs kids =
  optionalAttrs
    H.section_
    attrs
    True
    [ P.className "pb-4"
    ]
    [ H.p_
      [ P.classes_ ["text-sm"]
      ]
      kids
    ]
-----------------------------------------------------------------------------
accordionSample :: View m a
accordionSample =
  accordion_ []
    [ accordionSection_ []
      [ accordionHeader_ []
        [ "Is it accessible?" ]
      , accordionBody_ []
        [ "Yes. It adheres to the WAI-ARIA design pattern." ]
      ]
    , accordionSection_ []
      [ accordionHeader_ []
        [ "Is it styled?" ]
      , accordionBody_ []
        [ "Yes. It comes with default styles that match other component aesthetic." ]
      ]
    , accordionSection_ []
      [ accordionHeader_ []
        [ "Is it animated?" ]
      , accordionBody_ []
        [ "Yes. It's animated by default, but you can disable it if you prefer." ]
      ]
    ]
-----------------------------------------------------------------------------
accordionCodeSample :: View m a
accordionCodeSample = text
  """
  -----------------------------------------------------------------------------
  module MyAccordion (myAccordion) where
  -----------------------------------------------------------------------------
  import Miso
  import Miso.UI.Accordion
    ( accordion_
    , accordionSection_
    , accordionHeader_
    , accordionBody_
    )
  -----------------------------------------------------------------------------
  myAccordion :: View model action
  myAccordion = accordion_ []
    [ accordionSection_ []
      [ accordionHeader_ []
        [ "Is it accessible?" ]
      , accordionBody_ []
        [ "Yes. It adheres to the WAI-ARIA design pattern." ]
      ]
    , accordionSection_ []
      [ accordionHeader_ []
        [ "Is it styled?" ]
      , accordionBody_ []
        [ "Yes. It comes with default styles , that matches the other components aesthetic." ]
      ]
    , accordionSection_ []
      [ accordionHeader_ []
        [ "Is it animated?" ]
      , accordionBody_ []
        [ "Yes. It's animated by default, but you can disable it if you prefer." ]
      ]
    ]
  -----------------------------------------------------------------------------
  """
-----------------------------------------------------------------------------
