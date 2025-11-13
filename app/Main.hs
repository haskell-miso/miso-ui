-----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE MultilineStrings   #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad
import           Language.Javascript.JSaddle
import           Control.Category ((<<<))
import           Prelude hiding ((.))
-----------------------------------------------------------------------------
import           Miso.Html hiding (data_)
import qualified Miso.Html as H
import           Miso.Html.Property hiding (title_)
import           Miso.Svg.Element hiding (title_)
import qualified Miso.Svg.Element as S
import           Miso.Svg.Property hiding (id_, height_, width_, target_)
-----------------------------------------------------------------------------
import           Miso
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
      [ Href "/assets/styles.css"
      ]
  , scripts =
      [ Src "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.3/dist/js/all.min.js"
      , Src "https://basecoatui.com/assets/js/sidebar.js"
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
-----------------------------------------------------------------------------
data Model
  = Model
  { _someAlert :: UI.Alert
  , _someAlertDialog :: UI.AlertDialog
  } deriving Eq
-----------------------------------------------------------------------------
data Action = ToggleDarkMode | ChangeTheme MisoString | ToggleSidebar
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
app = component emptyModel update_ homeView
  where
    update_ = \case
      ToggleSidebar ->
        io_ $ do
          let event :: MisoString
              event = "document.dispatchEvent(new CustomEvent('basecoat:sidebar'))"
          void (eval event)
          consoleLog "clicked sidebar"
      ToggleDarkMode ->
        io_ toggleDarkMode
      ChangeTheme theme  -> do
        io_ $ do
          void $ eval ("""
            document.documentElement.classList.forEach(c => {
              if (c.startsWith('theme-')) {
                 document.documentElement.classList.remove(c);
              }
            });
          """ :: MisoString)
          void $ jsg ("document" :: MisoString)
             ! ("documentElement" :: MisoString)
             ! ("classList" :: MisoString)
             # ("add" :: MisoString) $ ["theme-" <> theme]
-----------------------------------------------------------------------------
toggleDarkMode :: JSM ()
toggleDarkMode = do
  doc <- jsg ("document" :: MisoString)
  event <-
    new (jsg ("CustomEvent" :: MisoString))
      ["basecoat:theme" :: MisoString]
  _ <- (doc # ("dispatchEvent" :: MisoString)) event
  pure ()
-----------------------------------------------------------------------------
homeView :: p -> View Model Action
homeView _ =
  div_ []
  [ asideView
  , mainContent
  ]
-----------------------------------------------------------------------------
mainContent :: View Model Action
mainContent =
  main_
    [id_ "content"]
    [ header_
        [ class_
            "bg-background sticky inset-x-0 top-0 isolate flex shrink-0 items-center gap-2 border-b z-10"
        ]
        [ div_
            [ class_ "flex h-14 w-full items-center gap-2 px-4"
            ]
            [ button_
                [ class_ "btn-sm-icon-ghost mr-auto size-7 -ml-1.5"
                , data_ "align" "start"
                , data_ "side" "bottom"
                , data_ "tooltip" "Toggle sidebar"
                , aria_ "label" "Toggle sidebar"
                , type_ "button"
                , onClick ToggleSidebar
                ]
                [ svg_
                    [ strokeLinejoin_ "round"
                    , strokeLinecap_ "round"
                    , strokeWidth_ "2"
                    , stroke_ "currentcolor"
                    , fill_ "none"
                    , viewBox_ "0 0 24 24"
                    , height_ "24"
                    , width_ "24"
                    , xmlns_ "http://www.w3.org/2000/svg"
                    ]
                    [ rect_
                        [ rx_ "2"
                        , y_ "3"
                        , x_ "3"
                        , height_ "18"
                        , width_ "18"
                        ]
                    , S.path_ [d_ "M9 3v18"]
                    ]
                ]
            , select_
                [ id_ "theme-select"
                , class_ "select h-8 leading-none"
                , onChange ChangeTheme
                ]
                [ option_ [value_ ""] ["Default"]
                , option_ [value_ "claude", selected_ True ] ["Claude"]
                , option_ [value_ "doom-64"] ["Doom 64"]
                , option_ [value_ "supabase"] ["Supabase"]
                ]
            , button_
                [ class_ "btn-icon-outline size-8"
                , data_ "side" "bottom"
                , data_ "tooltip" "Toggle dark mode"
                , aria_ "label" "Toggle dark mode"
                , type_ "button"
                , onClick ToggleDarkMode
                ]
                [ span_
                    [class_ "hidden dark:block"]
                    [ svg_
                        [ strokeLinejoin_ "round"
                        , strokeLinecap_ "round"
                        , strokeWidth_ "2"
                        , stroke_ "currentcolor"
                        , fill_ "none"
                        , viewBox_ "0 0 24 24"
                        , height_ "24"
                        , width_ "24"
                        , xmlns_ "http://www.w3.org/2000/svg"
                        ]
                        [ circle_ [r_ "4", cy_ "12", cx_ "12"]
                        , S.path_ [d_ "M12 2v2"]
                        , S.path_ [d_ "M12 20v2"]
                        , S.path_ [d_ "m4.93 4.93 1.41 1.41"]
                        , S.path_ [d_ "m17.66 17.66 1.41 1.41"]
                        , S.path_ [d_ "M2 12h2"]
                        , S.path_ [d_ "M20 12h2"]
                        , S.path_ [d_ "m6.34 17.66-1.41 1.41"]
                        , S.path_ [d_ "m19.07 4.93-1.41 1.41"]
                        ]
                    ]
                , span_
                    [class_ "block dark:hidden"]
                    [ svg_
                        [ strokeLinejoin_ "round"
                        , strokeLinecap_ "round"
                        , strokeWidth_ "2"
                        , stroke_ "currentcolor"
                        , fill_ "none"
                        , viewBox_ "0 0 24 24"
                        , height_ "24"
                        , width_ "24"
                        , xmlns_ "http://www.w3.org/2000/svg"
                        ]
                        [ S.path_
                            [d_ "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z"]
                        ]
                    ]
                ]
            , a_
                [ data_ "align" "end"
                , data_ "side" "bottom"
                , data_ "tooltip" "GitHub repository"
                , rel_ "noopener noreferrer"
                , target_ "_blank"
                , class_ "btn-icon size-8"
                , href_ "https://github.com/haskell-miso/miso-ui"
                ]
                [ svg_
                    [ strokeLinejoin_ "round"
                    , strokeLinecap_ "round"
                    , strokeWidth_ "2"
                    , stroke_ "currentcolor"
                    , fill_ "none"
                    , viewBox_ "0 0 24 24"
                    , height_ "24"
                    , width_ "24"
                    , xmlns_ "http://www.w3.org/2000/svg"
                    ]
                    [ S.path_
                        [ d_
                            "M15 22v-4a4.8 4.8 0 0 0-1-3.5c3 0 6-2 6-5.5.08-1.25-.27-2.48-1-3.5.28-1.15.28-2.35 0-3.5 0 0-1 0-3 1.5-2.64-.5-5.36-.5-8 0C6 2 5 2 5 2c-.3 1.15-.3 2.35 0 3.5A5.403 5.403 0 0 0 4 9c0 3.5 3 5.5 6 5.5-.39.49-.68 1.05-.85 1.65-.17.6-.22 1.23-.15 1.85v4"
                        ]
                    , S.path_ [d_ "M9 18c-4.51 2-5-2-7-2"]
                    ]
                ]
            ]
        ]
    , div_
        [class_ "p-4 md:p-6 xl:p-12"]
        [ div_
            [class_ "max-w-screen-lg mx-auto"]
            [ header_
                [class_ "flex flex-col gap-4"]
                [ div_
                    [class_ "flex flex-col gap-2"]
                    [ h1_
                        [ class_
                            "text-2xl font-bold tracking-tight sm:text-3xl md:text-4xl"
                        ]
                        ["Haskell miso 🍜 meets ShadCN"]
                    , p_
                        [class_ "sm:text-lg text-muted-foreground"]
                        [ "A miso Components library built with Tailwind, ShadCN and Basecoat CSS."
                        ]
                    ]
                , div_
                    [ class_
                        "flex w-full items-center justify-start gap-2 pt-2"
                    ]
                    [ a_
                        [href_ "/installation", class_ "btn"]
                        ["Get Started"]
                    , a_
                        [href_ "/introduction", class_ "btn-outline"]
                        ["Learn more"]
                    ]
                ]
            , section_
                [ data_ "protonpass-form" ""
                , class_
                    "grid grid-cols-1 lg:grid-cols-2 gap-4 mt-8"
                ]
                [ div_
                    [class_ "flex flex-col gap-4"]
                    [ div_
                        [class_ "card"]
                        [ header_
                            []
                            [ h2_ [] ["Team Members"]
                            , p_ [] ["Invite your team members to collaborate."]
                            ]
                        , section_
                            []
                            [ ul_
                                [class_ "grid gap-4"]
                                [ li_
                                    [class_ "flex items-center gap-4"]
                                    [ img_
                                        [ class_ "w-10 h-10 rounded-full"
                                        , alt_ "Sofia Davis"
                                        , src_ "/assets/images/avatar-1.png"
                                        ]
                                    , div_
                                        [class_ "flex flex-col gap-1 mr-auto"]
                                        [ h3_
                                            [class_ "text-sm font-medium leading-none"]
                                            ["Sofia Davis"]
                                        , p_
                                            [class_ "text-sm text-muted-foreground"]
                                            ["m@example.com"]
                                        ]
                                    , div_
                                        [ data_ "select-initialized" "true"
                                        , class_ "select"
                                        , id_ "select-201358"
                                        ]
                                        [ button_
                                            [ aria_ "controls" "select-201358-listbox"
                                            , aria_ "expanded" "false"
                                            , aria_ "haspopup" "listbox"
                                            , id_ "select-201358-trigger"
                                            , class_ "btn-outline justify-between font-normal"
                                            , type_ "button"
                                            ]
                                            [ span_ [class_ "truncate"] ["Owner"]
                                            , svg_
                                                [ class_
                                                    "lucide lucide-chevrons-up-down-icon lucide-chevrons-up-down text-muted-foreground opacity-50 shrink-0"
                                                , strokeLinejoin_ "round"
                                                , strokeLinecap_ "round"
                                                , strokeWidth_ "2"
                                                , stroke_ "currentcolor"
                                                , fill_ "none"
                                                , viewBox_ "0 0 24 24"
                                                , height_ "24"
                                                , width_ "24"
                                                , xmlns_ "http://www.w3.org/2000/svg"
                                                ]
                                                [ S.path_ [d_ "m7 15 5 5 5-5"]
                                                , S.path_ [d_ "m7 9 5-5 5 5"]
                                                ]
                                            ]
                                        , div_
                                            [ data_ "align" "end"
                                            , aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "select-201358-popover"
                                            ]
                                            [ header_
                                                []
                                                [ svg_
                                                    [ class_ "lucide lucide-search-icon lucide-search"
                                                    , strokeLinejoin_ "round"
                                                    , strokeLinecap_ "round"
                                                    , strokeWidth_ "2"
                                                    , stroke_ "currentcolor"
                                                    , fill_ "none"
                                                    , viewBox_ "0 0 24 24"
                                                    , height_ "24"
                                                    , width_ "24"
                                                    , xmlns_ "http://www.w3.org/2000/svg"
                                                    ]
                                                    [ circle_ [r_ "8", cy_ "11", cx_ "11"]
                                                    , S.path_ [d_ "m21 21-4.3-4.3"]
                                                    ]
                                                , input_
                                                    [ aria_ "labelledby" "select-201358-trigger"
                                                    , aria_ "controls" "select-201358-listbox"
                                                    , aria_ "expanded" "false"
                                                    , role_ "combobox"
                                                    , aria_ "autocomplete" "list"
                                                    , spellcheck_ False
                                                    , autocorrect_ False
                                                    , autocomplete_ False
                                                    , placeholder_ "Search entries..."
                                                    , textProp "value" ""
                                                    , type_ "text"
                                                    ]
                                                ]
                                            , div_
                                                [ aria_ "labelledby" "select-201358-trigger"
                                                , aria_ "orientation" "vertical"
                                                , id_ "select-201358-listbox"
                                                , role_ "listbox"
                                                ]
                                                [ div_
                                                    [ data_ "value" "viewer"
                                                    , role_ "option"
                                                    , id_ "select-201358-items-1"
                                                    ]
                                                    ["Viewer"]
                                                , div_
                                                    [ data_ "value" "developer"
                                                    , role_ "option"
                                                    , id_ "select-201358-items-2"
                                                    ]
                                                    ["Developer"]
                                                , div_
                                                    [ data_ "value" "billing"
                                                    , role_ "option"
                                                    , id_ "select-201358-items-3"
                                                    ]
                                                    ["Billing"]
                                                , div_
                                                    [ aria_ "selected" "true"
                                                    , data_ "value" "owner"
                                                    , role_ "option"
                                                    , id_ "select-201358-items-4"
                                                    ]
                                                    ["Owner"]
                                                ]
                                            ]
                                        , input_
                                            [ value_ "owner"
                                            , name_ "select-201358-value"
                                            , type_ "hidden"
                                            ]
                                        ]
                                    ]
                                , li_
                                    [class_ "flex items-center gap-4"]
                                    [ img_
                                        [ class_ "w-10 h-10 rounded-full"
                                        , alt_ "Jackson Lee"
                                        , src_ "/assets/images/avatar-2.png"
                                        ]
                                    , div_
                                        [class_ "flex flex-col gap-1 mr-auto"]
                                        [ h3_
                                            [class_ "text-sm font-medium leading-none"]
                                            ["Jackson Lee"]
                                        , p_
                                            [class_ "text-sm text-muted-foreground"]
                                            ["p@example.com"]
                                        ]
                                    , div_
                                        [ data_ "select-initialized" "true"
                                        , class_ "select"
                                        , id_ "select-929975"
                                        ]
                                        [ button_
                                            [ aria_ "controls" "select-929975-listbox"
                                            , aria_ "expanded" "false"
                                            , aria_ "haspopup" "listbox"
                                            , id_ "select-929975-trigger"
                                            , class_ "btn-outline justify-between font-normal"
                                            , type_ "button"
                                            ]
                                            [ span_ [class_ "truncate"] ["Empty"]
                                            , svg_
                                                [ class_
                                                    "lucide lucide-chevrons-up-down-icon lucide-chevrons-up-down text-muted-foreground opacity-50 shrink-0"
                                                , strokeLinejoin_ "round"
                                                , strokeLinecap_ "round"
                                                , strokeWidth_ "2"
                                                , stroke_ "currentcolor"
                                                , fill_ "none"
                                                , viewBox_ "0 0 24 24"
                                                , height_ "24"
                                                , width_ "24"
                                                , xmlns_ "http://www.w3.org/2000/svg"
                                                ]
                                                [ S.path_ [d_ "m7 15 5 5 5-5"]
                                                , S.path_ [d_ "m7 9 5-5 5 5"]
                                                ]
                                            ]
                                        , div_
                                            [ data_ "align" "end"
                                            , aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "select-929975-popover"
                                            ]
                                            [ header_
                                                []
                                                [ svg_
                                                    [ class_ "lucide lucide-search-icon lucide-search"
                                                    , strokeLinejoin_ "round"
                                                    , strokeLinecap_ "round"
                                                    , strokeWidth_ "2"
                                                    , stroke_ "currentcolor"
                                                    , fill_ "none"
                                                    , viewBox_ "0 0 24 24"
                                                    , height_ "24"
                                                    , width_ "24"
                                                    , xmlns_ "http://www.w3.org/2000/svg"
                                                    ]
                                                    [ circle_ [r_ "8", cy_ "11", cx_ "11"]
                                                    , S.path_ [d_ "m21 21-4.3-4.3"]
                                                    ]
                                                , input_
                                                    [ aria_ "labelledby" "select-929975-trigger"
                                                    , aria_ "controls" "select-929975-listbox"
                                                    , aria_ "expanded" "false"
                                                    , role_ "combobox"
                                                    , aria_ "autocomplete" "list"
                                                    , spellcheck_ False
                                                    , autocorrect_ False
                                                    , autocomplete_ False
                                                    , placeholder_ "Search entries..."
                                                    , textProp "value" ""
                                                    , type_ "text"
                                                    ]
                                                ]
                                            , div_
                                                [ aria_ "labelledby" "select-929975-trigger"
                                                , aria_ "orientation" "vertical"
                                                , id_ "select-929975-listbox"
                                                , role_ "listbox"
                                                ]
                                                [ div_
                                                    [ aria_ "selected" "true"
                                                    , data_ "value" ""
                                                    , role_ "option"
                                                    , id_ "select-929975-items-1"
                                                    ]
                                                    ["Empty"]
                                                , div_
                                                    [ data_ "value" "viewer"
                                                    , role_ "option"
                                                    , id_ "select-929975-items-2"
                                                    ]
                                                    ["Viewer"]
                                                , div_
                                                    [ data_ "value" "developer"
                                                    , role_ "option"
                                                    , id_ "select-929975-items-3"
                                                    ]
                                                    ["Developer"]
                                                , div_
                                                    [ data_ "value" "billing"
                                                    , role_ "option"
                                                    , id_ "select-929975-items-4"
                                                    ]
                                                    ["Billing"]
                                                , div_
                                                    [ data_ "value" "owner"
                                                    , role_ "option"
                                                    , id_ "select-929975-items-5"
                                                    ]
                                                    ["Owner"]
                                                ]
                                            ]
                                        , input_
                                            [ textProp "value" ""
                                            , name_ "select-929975-value"
                                            , type_ "hidden"
                                            ]
                                        ]
                                    ]
                                , li_
                                    [class_ "flex items-center gap-4"]
                                    [ img_
                                        [ class_ "w-10 h-10 rounded-full"
                                        , alt_ "Isabella Nguyen"
                                        , src_ "/assets/images/avatar-3.png"
                                        ]
                                    , div_
                                        [class_ "flex flex-col gap-1 mr-auto"]
                                        [ h3_
                                            [class_ "text-sm font-medium leading-none"]
                                            ["Isabella Nguyen"]
                                        , p_
                                            [class_ "text-sm text-muted-foreground"]
                                            ["i@example.com"]
                                        ]
                                    , div_
                                        [ data_ "select-initialized" "true"
                                        , class_ "select"
                                        , id_ "select-899025"
                                        ]
                                        [ button_
                                            [ aria_ "controls" "select-899025-listbox"
                                            , aria_ "expanded" "false"
                                            , aria_ "haspopup" "listbox"
                                            , id_ "select-899025-trigger"
                                            , class_ "btn-outline justify-between font-normal"
                                            , type_ "button"
                                            ]
                                            [ span_ [class_ "truncate"] ["Viewer"]
                                            , svg_
                                                [ class_
                                                    "lucide lucide-chevrons-up-down-icon lucide-chevrons-up-down text-muted-foreground opacity-50 shrink-0"
                                                , strokeLinejoin_ "round"
                                                , strokeLinecap_ "round"
                                                , strokeWidth_ "2"
                                                , stroke_ "currentcolor"
                                                , fill_ "none"
                                                , viewBox_ "0 0 24 24"
                                                , height_ "24"
                                                , width_ "24"
                                                , xmlns_ "http://www.w3.org/2000/svg"
                                                ]
                                                [ S.path_ [d_ "m7 15 5 5 5-5"]
                                                , S.path_ [d_ "m7 9 5-5 5 5"]
                                                ]
                                            ]
                                        , div_
                                            [ data_ "align" "end"
                                            , aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "select-899025-popover"
                                            ]
                                            [ header_
                                                []
                                                [ svg_
                                                    [ class_ "lucide lucide-search-icon lucide-search"
                                                    , strokeLinejoin_ "round"
                                                    , strokeLinecap_ "round"
                                                    , strokeWidth_ "2"
                                                    , stroke_ "currentcolor"
                                                    , fill_ "none"
                                                    , viewBox_ "0 0 24 24"
                                                    , height_ "24"
                                                    , width_ "24"
                                                    , xmlns_ "http://www.w3.org/2000/svg"
                                                    ]
                                                    [ circle_ [r_ "8", cy_ "11", cx_ "11"]
                                                    , S.path_ [d_ "m21 21-4.3-4.3"]
                                                    ]
                                                , input_
                                                    [ aria_ "labelledby" "select-899025-trigger"
                                                    , aria_ "controls" "select-899025-listbox"
                                                    , aria_ "expanded" "false"
                                                    , role_ "combobox"
                                                    , aria_ "autocomplete" "list"
                                                    , spellcheck_ False
                                                    , autocorrect_ False
                                                    , autocomplete_ False
                                                    , placeholder_ "Search entries..."
                                                    , textProp "value" ""
                                                    , type_ "text"
                                                    ]
                                                ]
                                            , div_
                                                [ aria_ "labelledby" "select-899025-trigger"
                                                , aria_ "orientation" "vertical"
                                                , id_ "select-899025-listbox"
                                                , role_ "listbox"
                                                ]
                                                [ div_
                                                    [ aria_ "selected" "true"
                                                    , data_ "value" "viewer"
                                                    , role_ "option"
                                                    , id_ "select-899025-items-1"
                                                    ]
                                                    ["Viewer"]
                                                , div_
                                                    [ data_ "value" "developer"
                                                    , role_ "option"
                                                    , id_ "select-899025-items-2"
                                                    ]
                                                    ["Developer"]
                                                , div_
                                                    [ data_ "value" "billing"
                                                    , role_ "option"
                                                    , id_ "select-899025-items-3"
                                                    ]
                                                    ["Billing"]
                                                , div_
                                                    [ data_ "value" "owner"
                                                    , role_ "option"
                                                    , id_ "select-899025-items-4"
                                                    ]
                                                    ["Owner"]
                                                ]
                                            ]
                                        , input_
                                            [ value_ "viewer"
                                            , name_ "select-899025-value"
                                            , type_ "hidden"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "card"]
                        [ header_
                            []
                            [ h2_ [] ["Cookie Settings"]
                            , p_ [] ["Manage your cookie settings here."]
                            ]
                        , section_
                            [class_ "text-sm grid gap-6"]
                            [ div_
                                [class_ "flex flex-col gap-0.5"]
                                [ div_
                                    [class_ "font-medium"]
                                    [ H.label_
                                        [ class_ "flex items-center justify-between gap-2"
                                        ]
                                        ["Strictly Necessary"]
                                    ]
                                , div_
                                    [class_ "text-muted-foreground"]
                                    [ H.label_
                                        [ class_ "flex items-center justify-between gap-2"
                                        ]
                                        [ "These cookies are essential in order to use the website and use its features."
                                        ]
                                    ]
                                ]
                            , H.label_
                                [ class_ "flex items-center justify-between gap-2"
                                ]
                                [ input_
                                    [ checked_ True
                                    , class_ "input"
                                    , role_ "switch"
                                    , type_ "checkbox"
                                    ]
                                ]
                            , div_
                                [class_ "flex flex-col gap-0.5"]
                                [ div_
                                    [class_ "font-medium"]
                                    [ H.label_
                                        [ class_ "flex items-center justify-between gap-2"
                                        ]
                                        ["Functional Cookies"]
                                    ]
                                , div_
                                    [class_ "text-muted-foreground"]
                                    [ H.label_
                                        [ class_ "flex items-center justify-between gap-2"
                                        ]
                                        [ "These cookies allow the website to provide personalized functionality."
                                        ]
                                    ]
                                ]
                            , H.label_
                                [ class_ "flex items-center justify-between gap-2"
                                ]
                                [ input_
                                    [ class_ "input"
                                    , role_ "switch"
                                    , type_ "checkbox"
                                    ]
                                ]
                            , div_
                                [class_ "flex flex-col gap-0.5"]
                                [ div_
                                    [class_ "font-medium"]
                                    [ H.label_
                                        [ class_ "flex items-center justify-between gap-2"
                                        ]
                                        ["Performance Cookies"]
                                    ]
                                , div_
                                    [class_ "text-muted-foreground"]
                                    [ H.label_
                                        [ class_ "flex items-center justify-between gap-2"
                                        ]
                                        [ "These cookies help to improve the performance of the website."
                                        ]
                                    ]
                                ]
                            , H.label_
                                [ class_ "flex items-center justify-between gap-2"
                                ]
                                [ input_
                                    [ class_ "input"
                                    , role_ "switch"
                                    , type_ "checkbox"
                                    ]
                                ]
                            ]
                        , footer_
                            []
                            [ button_
                                [class_ "btn-outline w-full", type_ "button"]
                                ["Save preferences"]
                            ]
                        ]
                    , div_
                        [class_ "card"]
                        [ header_
                            []
                            [ h2_ [] ["Payment Method"]
                            , p_ [] ["Add a new payment method to your account."]
                            ]
                        , section_
                            []
                            [ H.form
                                [class_ "form grid gap-6"]
                                [ ul_
                                    [class_ "flex gap-4"]
                                    [ li_
                                        [class_ "flex-1"]
                                        [ input_
                                            [ class_ "hidden peer"
                                            , id_ "payment-method-type-apple"
                                            , name_ "payment-method-type"
                                            , type_ "radio"
                                            ]
                                        , H.label_
                                            [ class_
                                                "text-sm font-medium leading-none flex flex-col items-center justify-between rounded-md border-2 border-muted p-4 hover:bg-muted peer-checked:border-primary [&>svg]:mb-3 [&>svg]:size-6"
                                            , for_ "payment-method-type-apple"
                                            ]
                                            [ svg_
                                                [ strokeLinejoin_ "round"
                                                , strokeLinecap_ "round"
                                                , strokeWidth_ "2"
                                                , stroke_ "currentcolor"
                                                , fill_ "none"
                                                , viewBox_ "0 0 24 24"
                                                , height_ "24"
                                                , width_ "24"
                                                , xmlns_ "http://www.w3.org/2000/svg"
                                                ]
                                                [ rect_
                                                    [ rx_ "2"
                                                    , y_ "5"
                                                    , x_ "2"
                                                    , height_ "14"
                                                    , width_ "20"
                                                    ]
                                                , line_
                                                    [y2_ "10", y1_ "10", x2_ "22", x1_ "2"]
                                                ]
                                            , "Card"
                                            ]
                                        ]
                                    , li_
                                        [class_ "flex-1"]
                                        [ input_
                                            [ class_ "hidden peer"
                                            , id_ "payment-method-type-card"
                                            , name_ "payment-method-type"
                                            , type_ "radio"
                                            ]
                                        , H.label_
                                            [ class_
                                                "text-sm font-medium leading-none flex flex-col items-center justify-between rounded-md border-2 border-muted p-4 hover:bg-muted peer-checked:border-primary [&>svg]:mb-3 [&>svg]:size-6"
                                            , for_ "payment-method-type-card"
                                            ]
                                            [ svg_
                                                [ xmlns_ "http://www.w3.org/2000/svg"
                                                , viewBox_ "0 0 24 24"
                                                , fill_ "currentcolor"
                                                , role_ "img"
                                                ]
                                                [ H.title_ [] ["PayPal"]
                                                , S.path_
                                                    [ d_
                                                        "M7.016 19.198h-4.2a.562.562 0 0 1-.555-.65L5.093.584A.692.692 0 0 1 5.776 0h7.222c3.417 0 5.904 2.488 5.846 5.5-.006.25-.027.5-.066.747A6.794 6.794 0 0 1 12.071 12H8.743a.69.69 0 0 0-.682.583l-.325 2.056-.013.083-.692 4.39-.015.087zM19.79 6.142c-.01.087-.01.175-.023.261a7.76 7.76 0 0 1-7.695 6.598H9.007l-.283 1.795-.013.083-.692 4.39-.134.843-.014.088H6.86l-.497 3.15a.562.562 0 0 0 .555.65h3.612c.34 0 .63-.249.683-.585l.952-6.031a.692.692 0 0 1 .683-.584h2.126a6.793 6.793 0 0 0 6.707-5.752c.306-1.95-.466-3.744-1.89-4.906z"
                                                    ]
                                                ]
                                            , "Paypal"
                                            ]
                                        ]
                                    , li_
                                        [class_ "flex-1"]
                                        [ input_
                                            [ class_ "hidden peer"
                                            , id_ "payment-method-type-paypal"
                                            , name_ "payment-method-type"
                                            , type_ "radio"
                                            ]
                                        , H.label_
                                            [ class_
                                                "text-sm font-medium leading-none flex flex-col items-center justify-between rounded-md border-2 border-muted p-4 hover:bg-muted peer-checked:border-primary [&>svg]:mb-3 [&>svg]:size-6"
                                            , for_ "payment-method-type-paypal"
                                            ]
                                            [ svg_
                                                [ xmlns_ "http://www.w3.org/2000/svg"
                                                , viewBox_ "0 0 24 24"
                                                , fill_ "currentcolor"
                                                , role_ "img"
                                                ]
                                                [ H.title_ [] ["Apple"]
                                                , S.path_
                                                    [ d_
                                                        "M12.152 6.896c-.948 0-2.415-1.078-3.96-1.04-2.04.027-3.91 1.183-4.961 3.014-2.117 3.675-.546 9.103 1.519 12.09 1.013 1.454 2.208 3.09 3.792 3.039 1.52-.065 2.09-.987 3.935-.987 1.831 0 2.35.987 3.96.948 1.637-.026 2.676-1.48 3.676-2.948 1.156-1.688 1.636-3.325 1.662-3.415-.039-.013-3.182-1.221-3.22-4.857-.026-3.04 2.48-4.494 2.597-4.559-1.429-2.09-3.623-2.324-4.39-2.376-2-.156-3.675 1.09-4.61 1.09zM15.53 3.83c.843-1.012 1.4-2.427 1.245-3.83-1.207.052-2.662.805-3.532 1.818-.78.896-1.454 2.338-1.273 3.714 1.338.104 2.715-.688 3.559-1.701"
                                                    ]
                                                ]
                                            , "Apple Pay"
                                            ]
                                        ]
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ H.label_ [for_ "payment-method-name"] ["Name"]
                                    , input_
                                        [ placeholder_ "John Doe"
                                        , id_ "payment-method-name"
                                        , type_ "text"
                                        ]
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ H.label_ [for_ "payment-method-city"] ["City"]
                                    , input_
                                        [ placeholder_ "New York"
                                        , id_ "payment-method-city"
                                        , type_ "text"
                                        ]
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ H.label_
                                        [for_ "payment-method-card-number"]
                                        ["Card Number"]
                                    , input_
                                        [ placeholder_ "1234 5678 9012 3456"
                                        , id_ "payment-method-card-number"
                                        , type_ "text"
                                        ]
                                    ]
                                , div_
                                    [class_ "flex gap-4"]
                                    [ div_
                                        [class_ "grid gap-2 flex-1"]
                                        [ H.label_
                                            [for_ "payment-method-expiration-month"]
                                            ["Expires"]
                                        , select_
                                            [ class_ "w-full"
                                            , id_ "payment-method-expiration-month"
                                            ]
                                            [ option_ [value_ "01"] ["January"]
                                            , option_ [value_ "02"] ["February"]
                                            , option_ [value_ "03"] ["March"]
                                            , option_ [value_ "04"] ["April"]
                                            , option_ [value_ "05"] ["May"]
                                            , option_ [value_ "06"] ["June"]
                                            , option_ [value_ "07"] ["July"]
                                            , option_ [value_ "08"] ["August"]
                                            , option_ [value_ "09"] ["September"]
                                            , option_ [value_ "10"] ["October"]
                                            , option_ [value_ "11"] ["November"]
                                            , option_ [value_ "12"] ["December"]
                                            ]
                                        ]
                                    , div_
                                        [class_ "grid gap-2 flex-1"]
                                        [ H.label_
                                            [for_ "payment-method-expiration-year"]
                                            ["Year"]
                                        , select_
                                            [ class_ "w-full"
                                            , id_ "payment-method-expiration-year"
                                            ]
                                            [ option_ [value_ "2024"] ["2024"]
                                            , option_ [value_ "2025"] ["2025"]
                                            , option_ [value_ "2026"] ["2026"]
                                            , option_ [value_ "2027"] ["2027"]
                                            , option_ [value_ "2028"] ["2028"]
                                            , option_ [value_ "2029"] ["2029"]
                                            , option_ [value_ "2030"] ["2030"]
                                            , option_ [value_ "2031"] ["2031"]
                                            , option_ [value_ "2032"] ["2032"]
                                            , option_ [value_ "2033"] ["2033"]
                                            , option_ [value_ "2034"] ["2034"]
                                            ]
                                        ]
                                    , div_
                                        [class_ "grid gap-2 flex-1"]
                                        [ H.label_ [for_ "payment-method-cvv"] ["CVV"]
                                        , input_
                                            [ class_ "w-full"
                                            , maxlength_ "4"
                                            , placeholder_ "123"
                                            , id_ "payment-method-cvv"
                                            , type_ "text"
                                            ]
                                        ]
                                    ]
                                , button_
                                    [class_ "btn w-full", type_ "button"]
                                    ["Continue"]
                                ]
                            ]
                        ]
                    ]
                , div_
                    [class_ "flex flex-col gap-4"]
                    [ div_
                        [class_ "card"]
                        [ section_
                            [class_ "space-y-6"]
                            [ header_
                                [class_ "flex items-center gap-2"]
                                [ img_
                                    [ class_ "w-10 h-10 rounded-full"
                                    , alt_ "Sofia Davis"
                                    , src_ "/assets/images/avatar-1.png"
                                    ]
                                , div_
                                    [class_ "flex flex-col gap-1 mr-auto"]
                                    [ h3_
                                        [class_ "text-sm font-medium leading-none"]
                                        ["Sofia Davis"]
                                    , p_
                                        [class_ "text-sm text-muted-foreground"]
                                        ["m@example.com"]
                                    ]
                                , button_
                                    [ data_ "tooltip" "New message"
                                    , class_ "btn-icon-outline rounded-full"
                                    , type_ "button"
                                    ]
                                    [ svg_
                                        [ strokeLinejoin_ "round"
                                        , strokeLinecap_ "round"
                                        , strokeWidth_ "2"
                                        , stroke_ "currentcolor"
                                        , fill_ "none"
                                        , viewBox_ "0 0 24 24"
                                        , height_ "24"
                                        , width_ "24"
                                        , xmlns_ "http://www.w3.org/2000/svg"
                                        ]
                                        [ S.path_ [d_ "M5 12h14"]
                                        , S.path_ [d_ "M12 5v14"]
                                        ]
                                    ]
                                ]
                            , section_
                                [class_ "space-y-4"]
                                [ div_
                                    [ class_
                                        "flex w-max max-w-[75%] flex-col gap-2 rounded-lg px-3 py-2 text-sm bg-muted"
                                    ]
                                    ["Hi, how can I help you today?"]
                                , div_
                                    [ class_
                                        "flex w-max max-w-[75%] flex-col gap-2 rounded-lg px-3 py-2 text-sm ml-auto bg-primary text-primary-foreground"
                                    ]
                                    ["Hey, I'm having trouble with my account."]
                                , div_
                                    [ class_
                                        "flex w-max max-w-[75%] flex-col gap-2 rounded-lg px-3 py-2 text-sm bg-muted"
                                    ]
                                    ["What seems to be the problem?"]
                                , div_
                                    [ class_
                                        "flex w-max max-w-[75%] flex-col gap-2 rounded-lg px-3 py-2 text-sm ml-auto bg-primary text-primary-foreground"
                                    ]
                                    ["I can't log in."]
                                ]
                            , footer_
                                [class_ "flex items-center gap-2"]
                                [ input_
                                    [ placeholder_ "Type your message here..."
                                    , class_ "input w-full"
                                    , type_ "text"
                                    ]
                                , button_
                                    [ textProp "disabled" ""
                                    , class_ "btn-icon"
                                    , type_ "button"
                                    ]
                                    [ svg_
                                        [ strokeLinejoin_ "round"
                                        , strokeLinecap_ "round"
                                        , strokeWidth_ "2"
                                        , stroke_ "currentcolor"
                                        , fill_ "none"
                                        , viewBox_ "0 0 24 24"
                                        , height_ "24"
                                        , width_ "24"
                                        , xmlns_ "http://www.w3.org/2000/svg"
                                        ]
                                        [ S.path_
                                            [ d_
                                                "M14.536 21.686a.5.5 0 0 0 .937-.024l6.5-19a.496.496 0 0 0-.635-.635l-19 6.5a.5.5 0 0 0-.024.937l7.93 3.18a2 2 0 0 1 1.112 1.11z"
                                            ]
                                        , S.path_ [d_ "m21.854 2.147-10.94 10.939"]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "card"]
                        [ header_
                            []
                            [ h2_ [] ["Create an account"]
                            , p_
                                []
                                ["Enter your email below to create your account"]
                            ]
                        , section_
                            [class_ "grid gap-6"]
                            [ div_
                                [class_ "flex gap-6"]
                                [ button_
                                    [class_ "btn-outline flex-1", type_ "button"]
                                    [ svg_
                                        [ xmlns_ "http://www.w3.org/2000/svg"
                                        , viewBox_ "0 0 24 24"
                                        , fill_ "currentcolor"
                                        , role_ "img"
                                        ]
                                        [ H.title_ [] ["GitHub"]
                                        , S.path_
                                            [ d_
                                                "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"
                                            ]
                                        ]
                                    , "GitHub"
                                    ]
                                , button_
                                    [class_ "btn-outline flex-1", type_ "button"]
                                    [ svg_
                                        [ xmlns_ "http://www.w3.org/2000/svg"
                                        , viewBox_ "0 0 24 24"
                                        , fill_ "currentcolor"
                                        , role_ "img"
                                        ]
                                        [ title_ [] ["Google"]
                                        , S.path_
                                            [ d_
                                                "M12.48 10.92v3.28h7.84c-.24 1.84-.853 3.187-1.787 4.133-1.147 1.147-2.933 2.4-6.053 2.4-4.827 0-8.6-3.893-8.6-8.72s3.773-8.72 8.6-8.72c2.6 0 4.507 1.027 5.907 2.347l2.307-2.307C18.747 1.44 16.133 0 12.48 0 5.867 0 .307 5.387.307 12s5.56 12 12.173 12c3.573 0 6.267-1.173 8.373-3.36 2.16-2.16 2.84-5.213 2.84-7.667 0-.76-.053-1.467-.173-2.053H12.48z"
                                            ]
                                        ]
                                    , "Google"
                                    ]
                                ]
                            , div_
                                [class_ "relative"]
                                [ div_
                                    [class_ "absolute inset-0 flex items-center"]
                                    []
                                , div_
                                    [ class_
                                        "relative flex justify-center text-xs uppercase"
                                    ]
                                    [ span_
                                        [class_ "bg-card px-2 text-muted-foreground"]
                                        ["Or continue with"]
                                    ]
                                ]
                            , form
                                [class_ "form grid gap-6"]
                                [ div_
                                    [class_ "grid gap-2"]
                                    [ H.label_ [for_ "demo-card-form-email"] ["Email"]
                                    , input_
                                        [id_ "demo-card-form-email", type_ "email"]
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ div_
                                        [class_ "flex items-center gap-2"]
                                        [ H.label_
                                            [for_ "demo-card-form-password"]
                                            ["Password"]
                                        , a_
                                            [ class_
                                                "ml-auto inline-block text-sm underline-offset-4 hover:underline"
                                            , href_ "#"
                                            ]
                                            ["Forgot your password?"]
                                        ]
                                    , input_
                                        [ id_ "demo-card-form-password"
                                        , type_ "password"
                                        ]
                                    ]
                                , button_
                                    [class_ "btn w-full", type_ "button"]
                                    ["Create an account"]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "card"]
                        [ header_
                            []
                            [ h2_ [] ["Report an issue"]
                            , p_ [] ["What area are you having problems with?"]
                            ]
                        , section_
                            []
                            [ form
                                [class_ "form grid gap-6"]
                                [ div_
                                    [class_ "flex gap-4"]
                                    [ div_
                                        [class_ "grid gap-2 flex-1"]
                                        [ H.label_ [for_ "report-issue-area"] ["Area"]
                                        , select_
                                            [class_ "w-full", id_ "report-issue-area"]
                                            [ option_ [value_ "team"] ["Team"]
                                            , option_ [value_ "billing"] ["Billing"]
                                            , option_ [value_ "account"] ["Account"]
                                            , option_ [value_ "deployments"] ["Deployments"]
                                            , option_ [value_ "support"] ["Support"]
                                            ]
                                        ]
                                    , div_
                                        [class_ "grid gap-2 flex-1"]
                                        [ H.label_
                                            [for_ "report-issue-security-level"]
                                            ["Security Level"]
                                        , select_
                                            [ class_ "w-full"
                                            , id_ "report-issue-security-level"
                                            ]
                                            [ option_ [value_ "1"] ["Severity 1 (Highest)"]
                                            , option_ [value_ "2"] ["Severity 2"]
                                            , option_ [value_ "3"] ["Severity 3"]
                                            , option_ [value_ "4"] ["Severity 4 (Lowest)"]
                                            ]
                                        ]
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ H.label_ [for_ "report-issue-subject"] ["Subject"]
                                    , input_
                                        [ class_ "w-full"
                                        , placeholder_ "I need help with..."
                                        , id_ "report-issue-subject"
                                        , type_ "text"
                                        ]
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ H.label_
                                        [for_ "report-issue-description"]
                                        ["Description"]
                                    , textarea_
                                        [ class_ "w-full"
                                        , placeholder_
                                            "Please include all information relevant to your issue."
                                        , id_ "report-issue-description"
                                        ]
                                        []
                                    ]
                                , footer_
                                    [ class_ "flex items-center gap-4 justify-between"
                                    ]
                                    [ button_
                                        [class_ "btn-sm-ghost", type_ "button"]
                                        ["Cancel"]
                                    , button_
                                        [class_ "btn-sm", type_ "button"]
                                        ["Continue"]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
-----------------------------------------------------------------------------
asideView :: View Model Action
asideView = aside_
    [ aria_ "hidden" "false"
    , data_ "side" "left"
    , class_ "sidebar"
    , id_ "sidebar"
    ]
    [ nav_
        [aria_ "label" "Sidebar navigation"]
        [ header_
            []
            [ a_
                [ class_ "btn-ghost p-2 h-12 w-full justify-start"
                , href_ "/"
                ]
                [ div_
                    [ class_
                        "bg-sidebar-primary text-sidebar-primary-foreground flex aspect-square size-8 items-center justify-center rounded-lg"
                    ]
                    [ svg_
                        [ class_ "h-4 w-4"
                        , viewBox_ "0 0 256 256"
                        , xmlns_ "http://www.w3.org/2000/svg"
                        ]
                        [ rect_
                            [fill_ "none", height_ "256", width_ "256"]
                        , line_
                            [ strokeWidth_ "32"
                            , strokeLinejoin_ "round"
                            , strokeLinecap_ "round"
                            , stroke_ "currentColor"
                            , fill_ "none"
                            , y2_ "208"
                            , x2_ "128"
                            , y1_ "128"
                            , x1_ "208"
                            ]
                        , line_
                            [ strokeWidth_ "32"
                            , strokeLinejoin_ "round"
                            , strokeLinecap_ "round"
                            , stroke_ "currentColor"
                            , fill_ "none"
                            , y2_ "192"
                            , x2_ "40"
                            , y1_ "40"
                            , x1_ "192"
                            ]
                        ]
                    ]
                , div_
                    [ class_
                        "grid flex-1 text-left text-sm leading-tight"
                    ]
                    [ span_
                        [class_ "truncate font-medium"]
                        ["miso-ui"]
                    , span_ [class_ "truncate text-xs"] ["v1.0.0"]
                    ]
                ]
            ]
        , section_
            [ class_
                "scrollbar [&_[data-new-link]::after]:content-['New'] [&_[data-new-link]::after]:ml-auto [&_[data-new-link]::after]:text-xs [&_[data-new-link]::after]:font-medium [&_[data-new-link]::after]:bg-sidebar-primary [&_[data-new-link]::after]:text-sidebar-primary-foreground [&_[data-new-link]::after]:px-2 [&_[data-new-link]::after]:py-0.5 [&_[data-new-link]::after]:rounded-md"
            ]
            [ div_
                [ aria_ "labelledby" "group-label-sidebar-content-1"
                , role_ "group"
                ]
                [ h3_
                    [id_ "group-label-sidebar-content-1"]
                    ["Getting started"]
                , ul_
                    []
                    [ li_
                        []
                        [ a_
                            [ href_ "/introduction"
                            ]
                            [ svg_
                                [ strokeLinejoin_ "round"
                                , strokeLinecap_ "round"
                                , strokeWidth_ "2"
                                , stroke_ "currentColor"
                                , fill_ "none"
                                , viewBox_ "0 0 24 24"
                                , height_ "24"
                                , width_ "24"
                                , xmlns_ "http://www.w3.org/2000/svg"
                                ]
                                [ circle_ [r_ "10", cy_ "12", cx_ "12"]
                                , S.path_ [d_ "M12 16v-4"]
                                , S.path_ [d_ "M12 8h.01"]
                                ]
                            , span_ [] ["Introduction"]
                            ]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/installation"
                            ]
                            [ svg_
                                [ strokeLinejoin_ "round"
                                , strokeLinecap_ "round"
                                , strokeWidth_ "2"
                                , stroke_ "currentColor"
                                , fill_ "none"
                                , viewBox_ "0 0 24 24"
                                , height_ "24"
                                , width_ "24"
                                , xmlns_ "http://www.w3.org/2000/svg"
                                ]
                                [ S.path_ [d_ "m7 11 2-2-2-2"]
                                , S.path_ [d_ "M11 13h4"]
                                , rect_
                                    [ ry_ "2"
                                    , rx_ "2"
                                    , y_ "3"
                                    , x_ "3"
                                    , height_ "18"
                                    , width_ "18"
                                    ]
                                ]
                            , span_ [] ["Installation"]
                            ]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/kitchen-sink"
                            ]
                            [ svg_
                                [ strokeLinejoin_ "round"
                                , strokeLinecap_ "round"
                                , strokeWidth_ "2"
                                , stroke_ "currentColor"
                                , fill_ "none"
                                , viewBox_ "0 0 24 24"
                                , height_ "24"
                                , width_ "24"
                                , xmlns_ "http://www.w3.org/2000/svg"
                                ]
                                [ rect_
                                    [ rx_ "1"
                                    , y_ "3"
                                    , x_ "3"
                                    , height_ "9"
                                    , width_ "7"
                                    ]
                                , rect_
                                    [ rx_ "1"
                                    , y_ "3"
                                    , x_ "14"
                                    , height_ "5"
                                    , width_ "7"
                                    ]
                                , rect_
                                    [ rx_ "1"
                                    , y_ "12"
                                    , x_ "14"
                                    , height_ "9"
                                    , width_ "7"
                                    ]
                                , rect_
                                    [ rx_ "1"
                                    , y_ "16"
                                    , x_ "3"
                                    , height_ "5"
                                    , width_ "7"
                                    ]
                                ]
                            , span_ [] ["Kitchen sink"]
                            ]
                        ]
                    , li_
                        []
                        [ a_
                            [ target_ "_blank"
                            , href_ "https://github.com/dmjio/miso-ui"
                            ]
                            [ svg_
                                [ xmlns_ "http://www.w3.org/2000/svg"
                                , viewBox_ "0 0 24 24"
                                , fill_ "currentColor"
                                ]
                                [ H.title_ [] ["GitHub"]
                                , S.path_
                                    [ d_
                                        "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"
                                    ]
                                ]
                            , span_ [] ["GitHub"]
                            ]
                        ]
                    , li_
                        []
                        [ a_
                            [ target_ "_blank"
                            , href_ "https://basecoatui.com/chat"
                            ]
                            [ svg_
                                [ xmlns_ "http://www.w3.org/2000/svg"
                                , viewBox_ "0 0 24 24"
                                , fill_ "currentColor"
                                ]
                                [ H.title_ [] ["Discord"]
                                , S.path_
                                    [ d_
                                        "M20.317 4.3698a19.7913 19.7913 0 00-4.8851-1.5152.0741.0741 0 00-.0785.0371c-.211.3753-.4447.8648-.6083 1.2495-1.8447-.2762-3.68-.2762-5.4868 0-.1636-.3933-.4058-.8742-.6177-1.2495a.077.077 0 00-.0785-.037 19.7363 19.7363 0 00-4.8852 1.515.0699.0699 0 00-.0321.0277C.5334 9.0458-.319 13.5799.0992 18.0578a.0824.0824 0 00.0312.0561c2.0528 1.5076 4.0413 2.4228 5.9929 3.0294a.0777.0777 0 00.0842-.0276c.4616-.6304.8731-1.2952 1.226-1.9942a.076.076 0 00-.0416-.1057c-.6528-.2476-1.2743-.5495-1.8722-.8923a.077.077 0 01-.0076-.1277c.1258-.0943.2517-.1923.3718-.2914a.0743.0743 0 01.0776-.0105c3.9278 1.7933 8.18 1.7933 12.0614 0a.0739.0739 0 01.0785.0095c.1202.099.246.1981.3728.2924a.077.077 0 01-.0066.1276 12.2986 12.2986 0 01-1.873.8914.0766.0766 0 00-.0407.1067c.3604.698.7719 1.3628 1.225 1.9932a.076.076 0 00.0842.0286c1.961-.6067 3.9495-1.5219 6.0023-3.0294a.077.077 0 00.0313-.0552c.5004-5.177-.8382-9.6739-3.5485-13.6604a.061.061 0 00-.0312-.0286zM8.02 15.3312c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9555-2.4189 2.157-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.9555 2.4189-2.1569 2.4189zm7.9748 0c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9554-2.4189 2.1569-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.946 2.4189-2.1568 2.4189Z"
                                    ]
                                ]
                            , span_ [] ["Discord"]
                            ]
                        ]
                    ]
                ]
            , div_
                [ aria_ "labelledby" "group-label-sidebar-content-2"
                , role_ "group"
                ]
                [ h3_
                    [id_ "group-label-sidebar-content-2"]
                    ["Components"]
                , ul_
                    []
                    [ li_
                        []
                        [ a_
                            [ href_ "/components/accordion"
                            ]
                            [span_ [] ["Accordion"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/alert"
                            ]
                            [span_ [] ["Alert"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/alert-dialog"
                            ]
                            [span_ [] ["Alert Dialog"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/avatar"
                            ]
                            [span_ [] ["Avatar"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/badge"
                            ]
                            [span_ [] ["Badge"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/breadcrumb"
                            ]
                            [span_ [] ["Breadcrumb"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/button"
                            ]
                            [span_ [] ["Button"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/button-group"
                            ]
                            [span_ [] ["Button Group"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/card"
                            ]
                            [span_ [] ["Card"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/checkbox"
                            ]
                            [span_ [] ["Checkbox"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/command"
                            ]
                            [span_ [] ["Command"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/combobox"
                            ]
                            [span_ [] ["Combobox"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/dialog"
                            ]
                            [span_ [] ["Dialog"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/dropdown-menu"
                            ]
                            [span_ [] ["Dropdown Menu"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/empty"
                            ]
                            [span_ [] ["Empty"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/field"
                            ]
                            [span_ [] ["Field"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/form"
                            ]
                            [span_ [] ["Form"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/input"
                            ]
                            [span_ [] ["Input"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/input-group"
                            ]
                            [span_ [] ["Input Group"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/item"
                            ]
                            [span_ [] ["Item"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/kbd"
                            ]
                            [span_ [] ["Kbd"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/label"
                            ]
                            [span_ [] ["Label"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/pagination"
                            ]
                            [span_ [] ["Pagination"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/popover"
                            ]
                            [span_ [] ["Popover"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/progress"
                            ]
                            [span_ [] ["Progress"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/radio-group"
                            ]
                            [span_ [] ["Radio Group"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/select"
                            ]
                            [span_ [] ["Select"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/sidebar"
                            ]
                            [span_ [] ["Sidebar"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/skeleton"
                            ]
                            [span_ [] ["Skeleton"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/slider"
                            ]
                            [span_ [] ["Slider"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ data_ "new-link" "true"
                            , href_ "/components/spinner"
                            ]
                            [span_ [] ["Spinner"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/switch"
                            ]
                            [span_ [] ["Switch"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/table"
                            ]
                            [span_ [] ["Table"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/tabs"
                            ]
                            [span_ [] ["Tabs"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/textarea"
                            ]
                            [span_ [] ["Textarea"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/theme-switcher"
                            ]
                            [span_ [] ["Theme Switcher"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/toast"
                            ]
                            [span_ [] ["Toast"]]
                        ]
                    , li_
                        []
                        [ a_
                            [ href_ "/components/tooltip"
                            ]
                            [span_ [] ["Tooltip"]]
                        ]
                    ]
                ]
            ]
        , footer_
            []
            [ div_
                [class_ "popover ", id_ "popover-925347"]
                [ button_
                    [ data_ "keep-mobile-sidebar-open" ""
                    , class_
                        "btn-ghost p-2 h-12 w-full flex items-center justify-start"
                    , aria_ "controls" "popover-925347-popover"
                    , aria_ "expanded" "false"
                    , type_ "button"
                    , id_ "popover-925347-trigger"
                    ]
                    [ img_
                        [ class_ "rounded-lg shrink-0 size-8"
                        , src_ "https://github.com/dmjio.png"
                        ]
                    , div_
                        [ class_
                            "grid flex-1 text-left text-sm leading-tight"
                        ]
                        [ span_
                            [class_ "truncate font-medium"]
                            ["David M. Johnson"]
                        , span_ [class_ "truncate text-xs"] ["@dmjio"]
                        ]
                    , svg_
                        [ strokeLinejoin_ "round"
                        , strokeLinecap_ "round"
                        , strokeWidth_ "2"
                        , stroke_ "currentColor"
                        , fill_ "none"
                        , viewBox_ "0 0 24 24"
                        , height_ "24"
                        , width_ "24"
                        , xmlns_ "http://www.w3.org/2000/svg"
                        ]
                        [ S.path_ [d_ "m7 15 5 5 5-5"]
                        , S.path_ [d_ "m7 9 5-5 5 5"]
                        ]
                    ]
                , div_
                    [ class_ "w-[271px] md:w-[239px]"
                    , data_ "side" "top"
                    , aria_ "hidden" "true"
                    , data_ "popover" ""
                    , id_ "popover-925347-popover"
                    ]
                    [ div_
                        [class_ "grid gap-4"]
                        [ header_
                            [class_ "grid gap-1.5"]
                            [ h2_
                                [class_ "font-semibold"]
                                ["I hope you like miso-ui ..."]
                            , p_
                                [class_ "text-muted-foreground text-sm"]
                                [ "My name is"
                                , a_
                                    [ target_ "_blank"
                                    , href_ "https://github.com/dmjio"
                                    ]
                                    ["@dmjio"]
                                , " and I'm using the Basecoat CSS framework from "
                                , a_
                                    [ target_ "_target"
                                    , href_ "https://github.com/hunvreus"
                                    , class_ "underline underline-offset-4"
                                    ]
                                    ["@hunvreus"]
                                , "). If you find it useful, please consider sponsoring or following @hunvreus"
                                ]
                            ]
                        , footer_
                            [class_ "grid gap-2"]
                            [ a_
                                [ target_ "_blank"
                                , class_ "btn-sm"
                                , href_ "https://github.com/hunvreus"
                                ]
                                ["Sponsor @hunvreus on GitHub"]
                            , a_
                                [ target_ "_blank"
                                , class_ "btn-sm-outline"
                                , href_ "https://x.com/hunvreus"
                                ]
                                ["Follow @hunvreus on X"]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
----------------------------------------------------------------------------
infixr 5 >>>
(>>>) :: Lens a b -> Lens b c -> Lens a c
(>>>) = flip (<<<)
----------------------------------------------------------------------------
