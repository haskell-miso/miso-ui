-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Tabs
  ( -- ** Views
    tabs_
  , tabList_
  , tabButton_
  , tab_
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
-----------------------------------------------------------------------------
-- | Must provide `_id`
tabs_
  :: [Attribute action]
  -> [View model action]
  -> View model action
tabs_ attrs kids =
  optionalAttrs
  H.div_
    attrs
    True
    [ P.class_ "tabs w-full"
    ]
    kids
-----------------------------------------------------------------------------
tabList_
  :: [Attribute action]
  -> [View model action]
  -> View model action
tabList_ attrs kids =
  optionalAttrs
  H.nav_
    attrs
    True
    [ P.class_ "w-full"
    , P.role_ "tablist"
    , P.aria_ "orientation" "horizontal"
    ]
    kids
-----------------------------------------------------------------------------
-- | The tab button
--
-- * `_id` is the TAB_ID (must match `aria_ "labelledby"` in `tab_`)
-- * `aria_ "control"` PANEL_ID. (must match `id_` of `tab_`)
-- * `tabIndex_` is used for keyboard navigation
-- * `aria_ "selected"` ("true" | "false")
--
tabButton_
  :: [Attribute action]
  -> [View model action]
  -> View model action
tabButton_ attrs kids =
  optionalAttrs
  H.button_
    attrs
    True
    [ -- SP.tabindex_ "0"
    -- , aria_ "selected" "true"
    -- , aria_ "controls" "demo-tabs-with-panels-panel-1"
    -- , id_ "demo-tabs-with-panels-tab-1"
      P.role_ "tab"
    , P.type_ "button"
    ]
    kids
-----------------------------------------------------------------------------
-- | Needs aria selected true
tab_
  :: [Attribute action]
  -> [View model action]
  -> View model action
tab_ attrs kids =
  optionalAttrs
  H.div_
    attrs
    True
    [ -- P.aria_ "selected" "true"
      -- SP.tabindex_ "-1"
--      P.aria_ "labelledby" "demo-tabs-with-panels-tab-1"
--    , P.id_ "demo-tabs-with-panels-panel-1"
      -- dmj: these must match tabButton_ "controls"
     P.role_ "tabpanel"
    ]
    kids
-----------------------------------------------------------------------------
