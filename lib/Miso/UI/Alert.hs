-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Alert
  ( -- ** Views
    alert_
  , alertHeader_
  , alertSection_
  , successIcon
  , destructiveIcon
    -- ** Sample
  , alertSample
  , alertCodeSample
  ) where
-----------------------------------------------------------------------------
import           Miso hiding (alert)
import qualified Miso.Svg as S
import qualified Miso.Svg.Property as SP
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
alert_
  :: [Attribute action]
  -> [View model action]
  -> View model action
alert_ attrs kids = do
  optionalAttrs
   H.div_
   attrs
   True
   [ P.class_ "alert" ]
   kids
-----------------------------------------------------------------------------
alertHeader_
  :: [Attribute action]
  -> [View model action]
  -> View model action
alertHeader_ attrs kids =
  optionalAttrs
  H.h2_
  attrs
  True
  []
  kids
-----------------------------------------------------------------------------
alertSection_
  :: [Attribute action]
  -> [View model action]
  -> View model action
alertSection_ attrs kids =
  optionalAttrs
  H.section_
  attrs
  True
  []
  kids
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
    , SP.r_ "10"
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
alertSample :: View model action
alertSample =
  H.div_
  [ P.class_ "p-4" ]
  [ H.div_ [ P.class_ "grid max-w-xl items-start gap-4" ]
    [ alert_ []
      [ successIcon
      , alertHeader_ [] [ "Success!" ]
      , alertSection_ []
        [ """
          Congratulations this is a
          successful alert !
          """
        ]
      ]
    , alert_
      [ P.class_ "alert-destructive" ]
      [ destructiveIcon
      , alertHeader_ [] [ "Warning!" ]
      , alertSection_ []
        [ """
          Something bad happened :( you're getting
          a destructive alert!
          """
        ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
alertCodeSample :: View model action
alertCodeSample =
  """
  -----------------------------------------------------------------------------
  module MyAlert (alertSample) where
  -----------------------------------------------------------------------------
  import           Miso
  import qualified Miso.Html as H
  import qualified Miso.Html.Property as P
  -----------------------------------------------------------------------------
  import qualified Miso.UI.Alert as Alert
  -----------------------------------------------------------------------------
  alertSample :: View model action
  alertSample =
    H.div_
    [ P.class_ "p-4" ]
    [ H.div_
      [ P.class_ "grid max-w-xl items-start gap-4"
      ]
      [ Alert.alert_ []
        [ Alert.successIcon
        , Alert.alertHeader_ [] [ "Success!" ]
        , Alert.alertSection_ []
          [ \"\"\"
            Congratulations this is a
            successful alert !
            \"\"\"
          ]
        ]
      , Alert.alert_
        [ P.class_ "alert-destructive" ]
        [ Alert.destructiveIcon
        , Alert.alertHeader_ [] [ "Warning!" ]
        , Alert.alertSection_ []
          [ \"\"\"
            Something bad happened :( you're getting
            a destructive alert!
            \"\"\"
          ]
        ]
      ]
    ]
  """
-----------------------------------------------------------------------------
