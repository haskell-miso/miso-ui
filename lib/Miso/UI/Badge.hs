-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultilineStrings           #-}
-----------------------------------------------------------------------------
module Miso.UI.Badge
  ( -- ** Views
    badge_
  , badgeSecondary_
  , badgeOutline_
  , badgeDestructive_
    -- ** Link Badges
  , badgeLink_
  , badgeLinkSecondary_
  , badgeLinkOutline_
  , badgeLinkDestructive_
    -- ** Rounded Badges
  , badgeRounded_
  , badgeRoundedDestructive_
  , badgeRoundedOutline_
    -- ** Samples
  , badgeSample
  , badgeCodeSample
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
badge_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badge_ attrs kids =
  H.span_ (attrs ++ [ P.class_ "badge" ]) kids
-----------------------------------------------------------------------------
badgeSecondary_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeSecondary_ attrs kids =
  badge_ (P.class_ "badge-secondary" : attrs) kids
-----------------------------------------------------------------------------
badgeOutline_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeOutline_ attrs kids =
  H.span_ (attrs ++ [ P.class_ "badge-outline" ]) kids
-----------------------------------------------------------------------------
badgeDestructive_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeDestructive_ attrs kids =
  badge_ (P.class_ "badge-destructive" : attrs) kids
-----------------------------------------------------------------------------
badgeLink_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeLink_ attrs kids =
  H.a_ (attrs ++ [ P.class_ "badge" ]) kids
-----------------------------------------------------------------------------
badgeLinkSecondary_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeLinkSecondary_ attrs kids =
  badgeLink_ (P.class_ "badge-secondary" : attrs) kids
-----------------------------------------------------------------------------
badgeLinkOutline_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeLinkOutline_ attrs kids =
  H.a_ (attrs ++ [ P.class_ "badge-outline" ]) kids
-----------------------------------------------------------------------------
badgeLinkDestructive_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeLinkDestructive_ attrs kids =
  badgeLink_ (P.class_ "badge-destructive" : attrs) kids
-----------------------------------------------------------------------------
badgeRounded_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeRounded_ attrs kids =
  badge_ (P.class_ "rounded-full min-w-5 px-1" : attrs) kids
-----------------------------------------------------------------------------
badgeRoundedDestructive_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeRoundedDestructive_ attrs kids =
  badgeDestructive_ (P.class_ "rounded-full min-w-5 px-1" : attrs) kids
-----------------------------------------------------------------------------
badgeRoundedOutline_
  :: [Attribute action]
  -> [View model action]
  -> View model action
badgeRoundedOutline_ attrs kids =
  badgeOutline_ (P.class_ "rounded-full min-w-5 px-1" : attrs) kids
-----------------------------------------------------------------------------
badgeSample :: View model action
badgeSample =
  H.div_
  [ P.class_ "flex flex-col gap-2" ]
  [ H.div_
    [ P.class_ "flex flex-wrap items-center gap-2 md:flex-row" ]
    [ badge_ [] ["Primary"]
    , badgeSecondary_ [] ["Secondary"]
    , badgeOutline_ [] ["Outline"]
    , badgeDestructive_ [] ["Destructive"]
    , badgeRounded_ [] ["8"]
    , badgeRoundedDestructive_ [] ["99"]
    , badgeRoundedOutline_ [ P.class_ "font-mono tabular-nums" ] ["20+"]
    ]
  , H.div_
    [ P.class_ "flex flex-wrap items-center gap-2 md:flex-row" ]
    [ badgeLink_ [ P.href_ "#" ] [ "Link" ]
    , badgeLinkSecondary_ [ P.href_ "#" ] [ "Link" ]
    , badgeLinkDestructive_ [ P.href_ "#" ] [ "Link" ]
    , badgeLinkOutline_ [ P.href_ "#" ] [ "Link" ]
    ]
  ]
-----------------------------------------------------------------------------
badgeCodeSample :: View model action
badgeCodeSample =
  """
  -----------------------------------------------------------------------------
  module MyBadge (badgeSample) where
  -----------------------------------------------------------------------------
  import           Miso
  import qualified Miso.Html as H
  import qualified Miso.Html.Property as P
  -----------------------------------------------------------------------------
  import qualified Miso.UI.Badge as Badge
  -----------------------------------------------------------------------------
  badgeSample :: View model action
  badgeSample =
    H.div_
    [ P.class_ "flex flex-col gap-2" ]
    [ H.div_
      [ P.class_ "flex flex-wrap items-center gap-2 md:flex-row" ]
      [ Badge.badge_ [] ["Primary"]
      , Badge.badgeSecondary_ [] ["Secondary"]
      , Badge.badgeOutline_ [] ["Outline"]
      , Badge.badgeDestructive_ [] ["Destructive"]
      , Badge.badgeRounded_ [] ["8"]
      , Badge.badgeRoundedDestructive_ [] ["99"]
      , Badge.badgeRoundedOutline_ [ P.class_ "font-mono tabular-nums" ] ["20+"]
      ]
    , H.div_
      [ P.class_ "flex flex-wrap items-center gap-2 md:flex-row" ]
      [ Badge.badgeLink_ [ P.href_ "#" ] [ "Link" ]
      , Badge.badgeLinkSecondary_ [ P.href_ "#" ] [ "Link" ]
      , Badge.badgeLinkDestructive_ [ P.href_ "#" ] [ "Link" ]
      , Badge.badgeLinkOutline_ [ P.href_ "#" ] [ "Link" ]
      ]
    ]
  """
