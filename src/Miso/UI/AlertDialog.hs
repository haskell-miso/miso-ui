-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.AlertDialog
  ( -- ** Component
    alertDialog_
  , alertDialogComponent
    -- ** Types
  , AlertDialog (..)
    -- ** Constructors
  , emptyAlertDialog
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
import           Language.Javascript.JSaddle ((#), jsg)
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
data AlertDialog
  = AlertDialog
  { _alertDialogButton :: MisoString
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
emptyAlertDialog :: AlertDialog
emptyAlertDialog = AlertDialog mempty
-----------------------------------------------------------------------------
alertDialog_ :: Binding parent AlertDialog -> Component parent AlertDialog Action
alertDialog_ binding = alertDialogComponent { bindings = pure binding }
-----------------------------------------------------------------------------
data Action
  = ShowModal DOMRef
  | CloseDialog
-----------------------------------------------------------------------------
alertDialogComponent :: Component parent AlertDialog Action
alertDialogComponent = vcomp
  where
    update_ (ShowModal domRef) = io_ $ do
      dialogRef <- nextSibling domRef
      void $ dialogRef # ("showModal" :: MisoString) $ ()

    update_ CloseDialog = io_ $ do
      dialog <- jsg @MisoString "document"
        # ("getElementById" :: MisoString)
        $ ["alert-dialog-demo" :: MisoString]
      void $ dialog # ("close" :: MisoString) $ ()

    vcomp = component emptyAlertDialog update_ $ \_ ->
      H.div_
      []
      [ H.button_
        [ P.type_ "button"
        , P.class_ "btn-outline"
        , E.onClickWith ShowModal
        ]
        [ "Open alert dialog"
        ]
      , H.dialog_
          [ P.id_ "alert-dialog-demo"
          , P.class_ "dialog"
          , P.aria_ "labelledby" "alert-dialog-title"
          , P.aria_ "describedby" "alert-dialog-description"
          ]
          [ H.article_ []
            [ H.header_ []
              [ H.h2_
                [ P.id_ "alert-dialog-title"
                ]
                [ "Are you absolutely sure?"
                ]
              , H.p_
                [ P.id_ "alert-dialog-description"
                ]
                [ text $
                    "This action cannot be undone. " <>
                    "This will permanently delete your" <>
                    "account and remove your data from our servers."
                ]
              ]
            , H.footer_ []
              [ H.button_
                [ P.class_ "btn-outline"
                , E.onClick CloseDialog
                ] [ "Cancel" ]
              , H.button_
                [ P.class_ "btn-primary"
                , E.onClick CloseDialog
                ] [ "Continue" ]
              ]
            ]
          ]
        ]
-----------------------------------------------------------------------------
