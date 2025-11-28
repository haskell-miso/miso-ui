-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Alert
  ( -- ** Component
    alert_
  , alertComponent
    -- ** Types
  , Alert (..)
  , AlertIcon (..)
    -- ** Constructors
  , emptyAlert
  , alertSuccess
  , alertDestructive
  ) where
-----------------------------------------------------------------------------
import           Miso hiding (alert)
import qualified Miso.Svg as S
import qualified Miso.Svg.Property as SP
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.Lens
-----------------------------------------------------------------------------
data Alert
  = Alert
  { _alertIcon :: Maybe AlertIcon
    -- ^ Optional icon
  , _alertHeader :: MisoString
    -- ^ Header of the alert <h2>
  , _alertSection :: MisoString
    -- ^ Sub section of the alert <section>
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
alertIcon :: Lens Alert (Maybe AlertIcon)
alertIcon = lens _alertIcon $ \r x -> r { _alertIcon = x }
-----------------------------------------------------------------------------
alertHeader :: Lens Alert MisoString
alertHeader = lens _alertHeader $ \r x -> r { _alertHeader = x }
-----------------------------------------------------------------------------
alertSection :: Lens Alert MisoString
alertSection = lens _alertSection $ \r x -> r { _alertSection = x }
-----------------------------------------------------------------------------
data AlertIcon
  = AlertSuccessful
  | AlertDestructive
  deriving (Show, Eq)
-----------------------------------------------------------------------------
alert_ :: Binding parent Alert -> Component parent Alert ()
alert_ binding = alertComponent { bindings = [binding] }
-----------------------------------------------------------------------------
emptyAlert :: Alert
emptyAlert = Alert Nothing mempty mempty 
-----------------------------------------------------------------------------
alertSuccess, alertDestructive :: MisoString -> MisoString -> Alert
alertSuccess = Alert (pure AlertSuccessful)
alertDestructive = Alert (pure AlertDestructive)
-----------------------------------------------------------------------------
alertComponent :: Component parent Alert ()
alertComponent = component emptyAlert noop $ \model ->
  H.div_ [ P.class_ "alert" ] (icon model <> textSection model)
    where
      icon model = 
        [ iconView ico
        | Just ico <- pure (model ^. alertIcon)
        ] where
            iconView = \case
              AlertSuccessful ->
                successIcon
              AlertDestructive ->
                destructiveIcon
      textSection model = 
        [ H.h2_ [] [ text (model ^. alertHeader) ]
        , H.section_ [] [ text (model ^. alertSection) ]
        ]
-----------------------------------------------------------------------------
successIcon :: View model action
successIcon = S.svg_
  [ P.xmlns_ "http://www.w3.org/2000/svg"
  , P.width_ "24"
  , P.height_ "24"
  , SP.viewBox_ "0 0 24 24"
  , SP.fill_ "none"
  , SP.stroke_ "currentColor"
  , SP.strokeWidth_ "2"
  , SP.strokeLinecap_ "round" 
  , SP.strokeLinejoin_ "round"
  ]
  [ S.circle_
    [ SP.cx_ "12"
    , SP.cy_ "12"
    , SP.r_ "S"
    ]
  , S.path_
    [ SP.d_ "m9 12 2 2 4-4"
    ]
  ]
-----------------------------------------------------------------------------
destructiveIcon :: View model action
destructiveIcon =
  S.svg_
    [ SP.strokeWidth_ "2"
    , SP.strokeLinecap_ "round"
    , SP.strokeLinejoin_ "round"
    , SP.stroke_ "currentColor"
    , SP.fill_ "none"
    , SP.viewBox_ "0 0 24 24"
    , P.height_ "24"
    , P.width_ "24"
    , P.xmlns_ "http://www.w3.org/2000/svg"
    ]
    [ S.circle_
      [ SP.r_ "10"
      , SP.cy_ "12"
      , SP.cx_ "12"
      ]
    , S.line_
      [ SP.y2_ "12"
      , SP.y1_ "8"
      , SP.x2_ "12"
      , SP.x1_ "12"
      ]
    , S.line_
      [ SP.y2_ "16"
      , SP.y1_ "16"
      , SP.x2_ "12.01"
      , SP.x1_ "12"
      ]
    ]
-----------------------------------------------------------------------------
