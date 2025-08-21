-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Accordion
  ( -- ** Component
    accordion_
    -- ** Types
  , Accordion (..)
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Svg as S
import qualified Miso.Svg.Property as SP
-----------------------------------------------------------------------------
data Accordion
  = Accordion
  { _accordionHeader :: MisoString
    -- ^ Header of the accordion <h2>
  , _accordionSection :: MisoString
    -- ^ Sub section of the accordion <section>
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
accordion_ :: Binding parent Accordion -> Component parent Accordion ()
accordion_ binding = accordionComponent { bindings = [binding] }
-----------------------------------------------------------------------------
accordionComponent :: Component parent Accordion ()
accordionComponent = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = section_
    [class_ "accordion"]
    [ details_
        [class_ "group border-b last:border-b-0"]
        [ summary_
            [ class_
                "w-full focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] transition-all outline-none rounded-md"
            ]
            [ h2_
                [ class_
                    "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline"
                ]
                [ "Is it accessible?"
                , svg_
                    [ class_
                        "text-muted-foreground pointer-events-none size-4 shrink-0 translate-y-0.5 transition-transform duration-200 group-open:rotate-180"
                    , textProp "stroke-linejoin" "round"
                    , textProp "stroke-linecap" "round"
                    , textProp "stroke-width" "2"
                    , S.stroke_ "currentColor"
                    , S.fill_ "none"
                    , S.viewBox_ "0 0 24 24"
                    , height_ "24"
                    , width_ "24"
                    , xmlns_ "http://www.w3.org/2000/svg"
                    ]
                    [S.path_ [SP.d_ "m6 9 6 6 6-6"] []]
                ]
            ]
        , section_
            [class_ "pb-4"]
            [ p_
                [class_ "text-sm"]
                [ "Yes. It adheres to the WAI-ARIA design pattern."
                ]
            ]
        ]
    , details_
        [class_ "group border-b last:border-b-0"]
        [ summary_
            [ class_
                "w-full focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] transition-all outline-none rounded-md"
            ]
            [ h2_
                [ class_
                    "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline"
                ]
                [ "Is it styled?"
                , svg_
                    [ class_
                        "text-muted-foreground pointer-events-none size-4 shrink-0 translate-y-0.5 transition-transform duration-200 group-open:rotate-180"
                    , textProp "stroke-linejoin" "round"
                    , textProp "stroke-linecap" "round"
                    , textProp "stroke-width" "2"
                    , S.stroke_ "currentColor"
                    , S.fill_ "none"
                    , S.viewBox_ "0 0 24 24"
                    , height_ "24"
                    , width_ "24"
                    , xmlns_ "http://www.w3.org/2000/svg"
                    ]
                    [S.path_ [S.d_ "m6 9 6 6 6-6"] []]
                ]
            ]
        , section_
            [class_ "pb-4"]
            [ p_
                [class_ "text-sm"]
                [ "Yes. It comes with default styles that matches the other components' aesthetic."
                ]
            ]
        ]
    , details_
        [class_ "group border-b last:border-b-0"]
        [ summary_
            [ class_
                "w-full focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] transition-all outline-none rounded-md"
            ]
            [ h2_
                [ class_
                    "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline"
                ]
                [ "Is it animated?"
                , svg_
                    [ class_
                        "text-muted-foreground pointer-events-none size-4 shrink-0 translate-y-0.5 transition-transform duration-200 group-open:rotate-180"
                    , textProp "stroke-linejoin" "round"
                    , textProp "stroke-linecap" "round"
                    , textProp "stroke-width" "2"
                    , S.stroke_ "currentColor"
                    , S.fill_ "none"
                    , S.viewBox_ "0 0 24 24"
                    , height_ "24"
                    , width_ "24"
                    , xmlns_ "http://www.w3.org/2000/svg"
                    ]
                    [S.path_ [SP.d_ "m6 9 6 6 6-6"] []]
                ]
            ]
        , section_
            []
            [ p_
                [class_ "text-sm whitespace-pre-wrap"]
                [ "Yes. It's animated by default, but you can disable it if you prefer."
                ]
            ]
        ]
    ]
