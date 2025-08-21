-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Card
  ( -- ** Component
    card_
  ) where
-----------------------------------------------------------------------------
import           Miso
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
card_ :: Component parent model action
card_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ =
  div_
  [ class_ "card w-full" ]
  [ header_ []
    [ h2_ [][ "Login to your account" ]
    , p_ [][ "Enter your details below to login to your account" ]
    ]
  , section_ []
    [ H.form
      [ class_ "form grid gap-6"
      ]
      [ div_
        [ class_ "grid gap-2" ]
        [ label_ [ for_ "demo-card-form-email" ][ "Email" ]
        , input_
          [ type_ "email"
          , id_ "demo-card-form-email"
          ]
        ]
      , div_ [ class_ "grid gap-2" ]
        [ div_ [ class_ "flex items-center gap-2" ]
          [ label_ [ for_ "demo-card-form-password" ][ "Password" ]
          , a_
            [ href_ "#"
            , class_ "ml-auto inline-block text-sm underline-offset-4 hover:underline"
            ] [ "Forgot your password?" ]
          ]
        , input_
          [ type_ "password"
          , id_ "demo-card-form-password"
          ]
        ]
      ]
    ]
  , footer_ [ class_ "flex flex-col items-center gap-2" ]
    [ button_
      [ type_ "button"
      , class_ "btn w-full"
      ] [ "Login" ]
    , button_
      [ type_ "button"
      , class_ "btn-outline w-full"
      ] [ "Login with Google" ]
    , p_ [ class_ "mt-4 text-center text-sm" ]
      [ "Don't have an account?"
      , a_
        [ href_ "#"
        , class_ "underline-offset-4 hover:underline"
        ] [ "Sign up" ]
      ]
    ]
  ]

