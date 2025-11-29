-----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE MultilineStrings   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingStrategies #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad
import           Language.Javascript.JSaddle
import           Prelude hiding ((.))
-----------------------------------------------------------------------------
import           Miso.Html hiding (data_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Html.Property hiding (title_, label_, href_, form_)
import           Miso.Svg.Element hiding (title_)
import qualified Miso.Svg.Element as S
import           Miso.Svg.Property hiding (target_)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
-----------------------------------------------------------------------------
import           Client (app)
-----------------------------------------------------------------------------
#ifndef GHCJS_BOTH
#ifdef WASM
import qualified Language.Javascript.JSaddle.Wasm.TH as JSaddle.Wasm.TH
-----------------------------------------------------------------------------
foreign export javascript "hs_start" main :: IO ()
#else
import           Data.FileEmbed (embedStringFile)
#endif
#endif
-----------------------------------------------------------------------------
withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef GHCJS_BOTH
#ifdef WASM
  $(JSaddle.Wasm.TH.evalFile "js/util.js")
#else
  _ <- eval ($(embedStringFile "js/util.js") :: MisoString)
#endif
#endif
  action
-----------------------------------------------------------------------------
main :: IO ()
main = run $ withJS (startApp app)
#ifdef VANILLA
  { styles =
      [ Href "/assets/styles.css"
      , Href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/default.min.css"
      ]
  , scripts =
      [ Src "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.6/dist/js/basecoat.min.js"
      , Src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"
      , Src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/haskell.min.js"
      , Script
        """
         (() => {
           try {
             const stored = localStorage.getItem('themeMode');
             if (stored ? stored === 'dark'
                        : matchMedia('(prefers-color-scheme: dark)').matches) {
               document.documentElement.classList.add('dark');
             }
           } catch (_) {}
           const apply = dark => {
             document.documentElement.classList.toggle('dark', dark);
             try { localStorage.setItem('themeMode', dark ? 'dark' : 'light'); } catch (_) {}
           };
           document.addEventListener('basecoat:theme', (event) => {
             const mode = event.detail?.mode;
             apply(mode === 'dark' ? true
                  : mode === 'light' ? false
                  : !document.documentElement.classList.contains('dark'));
           });
         })();
        """
      ]
  }
#endif
