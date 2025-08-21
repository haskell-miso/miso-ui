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
import           Miso hiding (alert)
-----------------------------------------------------------------------------
data AlertDialog
  = AlertDialog
  { _alertDialogButton :: MisoString
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
-- test :: IO ()
-- test = run (startComponent alertDialogComponent)
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
        $ ["alert-dialog" :: MisoString]
      void $ dialog # ("close" :: MisoString) $ ()

    vcomp = component emptyAlertDialog update_ $ \_ ->
      div_
      [
      ]
      [ button_
        [ type_ "button"
        , class_ "btn-outline"
        , onClickWith ShowModal
        ]
        [ "Open alert dialog"
        ]
      , dialog_ 
          [ id_ "alert-dialog" 
          , class_ "dialog" 
          , aria_ "labelledby" "alert-dialog-title" 
          , aria_ "describedby" "alert-dialog-description" 
          ] 
          [ article_ []
            [ header_ []
              [ h2_
                [ id_ "alert-dialog-title"
                ]
                [ "Are you absolutely sure?"
                ]
              , p_
                [ id_ "alert-dialog-description"
                ]
                [ text $
                    "This action cannot be undone. " <>
                    "This will permanently delete your" <>
                    "account and remove your data from our servers."
                ]
              ]
            , footer_ []
              [ button_ 
                [ class_ "btn-outline" 
                , onClick CloseDialog
                ] [ "Cancel" ]
              , button_ 
                [ class_ "btn-primary" 
                , onClick CloseDialog
                ] [ "Continue" ]
              ] 
            ]
          ]
        ]
-----------------------------------------------------------------------------
