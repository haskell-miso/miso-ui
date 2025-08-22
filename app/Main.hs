-----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Category ((<<<))
import           Prelude hiding ((.))
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
import           Miso.Lens
import           Miso.Lens.TH
-----------------------------------------------------------------------------
import qualified Miso.UI as UI
import           Miso.UI (Alert(..))
-----------------------------------------------------------------------------
$(makeLenses ''Alert)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startComponent app
#ifndef WASM
  { styles =
    [ Href "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.2/dist/basecoat.cdn.min.css"
    ]
  , scripts =
      [ Src "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.2/dist/js/all.min.js"
      , Src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"
      ]
  }
#endif
-----------------------------------------------------------------------------
data Model
  = Model
  { _someAlert :: UI.Alert
  , _someAlertDialog :: UI.AlertDialog
  } deriving Eq
-----------------------------------------------------------------------------
data Action = Toggle
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel =
  Model
    (UI.alertDestructive "Failure" "you did it wrong")
    UI.emptyAlertDialog
-----------------------------------------------------------------------------
someAlert :: Lens Model UI.Alert
someAlert = lens _someAlert $ \r x -> r { _someAlert = x }
-----------------------------------------------------------------------------
someAlertDialog :: Lens Model UI.AlertDialog
someAlertDialog = lens _someAlertDialog $ \r x -> r { _someAlertDialog = x }
-----------------------------------------------------------------------------
app :: App Model Action
app = component emptyModel update_ $ \_ ->
  div_
  []
  [ button_
    [ className "btn"
    , onClick Toggle
    ]
    [ "toggle alert icon"
    ]
  , div_ [ key_ @MisoString "alert" ] +>
      UI.alert_ (someAlert --> this)
  , div_ [ key_ @MisoString "alert-dialog" ] +>
      UI.alertDialog_ (someAlertDialog --> this)
  -- , avatar_
  -- , badge_
  -- , button__
  -- , card_
  -- , checkbox_
  -- , combobox_
  -- , tooltip_
  -- , textarea
  -- , popover
  ] where
      update_ = \case
        Toggle -> do
          someAlert %= \case
            Alert (Just UI.AlertDestructive) _ _ ->
              UI.alertSuccess "you did it" "nice"
            Alert (Just UI.AlertSuccessful) _ _ ->
              UI.alertDestructive "bad, fail" "nope"
            x -> x
----------------------------------------------------------------------------
infixr 5 >>>
(>>>) :: Lens a b -> Lens b c -> Lens a c
(>>>) = flip (<<<)
----------------------------------------------------------------------------
