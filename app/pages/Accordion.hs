{-# LANGUAGE OverloadedStrings #-}
module Accordion where

import Miso.Types
import Miso.Html hiding (data_)
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (id_, height_, width_, path_)

accordionPage :: View model action
accordionPage =
  div_
    [class_ "p-4 md:p-6 xl:p-12"]
    [ main_
        [ class_
            "mx-auto relative flex w-full max-w-screen-lg gap-10"
        ]
        [ div_
            [class_ "mx-auto w-full flex-1 max-w-screen-md"]
            [ header_
                [class_ "space-y-2"]
                [ h1_
                    [ class_
                        "text-2xl font-semibold tracking-tight sm:text-3xl xl:text-4xl"
                    ]
                    ["Accordion"]
                , p_
                    [ class_
                        "text-muted-foreground text-[1.05rem] sm:text-base"
                    ]
                    [ "A vertically stacked set of interactive headings that each reveal a section of content."
                    ]
                ]
            , article_
                [class_ "pb-12 mt-8 content"]
                [ div_
                    [class_ "alert mb-6"]
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
                        , line_
                            [y2_ "12", y1_ "8", x2_ "12", x1_ "12"]
                        , line_
                            [y2_ "16", y1_ "16", x2_ "12.01", x1_ "12"]
                        ]
                    , h2_
                        []
                        [ "There is no dedicated Accordion component in Basecoat."
                        ]
                    ]
                , div_
                    [class_ "relative my-6"]
                    [ div_
                        [ data_ "tabs-initialized" "true"
                        , id_ "tabs-895417"
                        , class_ "tabs "
                        ]
                        [ nav_
                            [ aria_ "orientation" "horizontal"
                            , role_ "tablist"
                            ]
                            [ button_
                                [ tabindex_ "0"
                                , aria_ "selected" "true"
                                , aria_ "controls" "tabs-895417-panel-1"
                                , id_ "tabs-895417-tab-1"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Preview"]
                            , button_
                                [ tabindex_ "-1"
                                , aria_ "selected" "false"
                                , aria_ "controls" "tabs-895417-panel-2"
                                , id_ "tabs-895417-tab-2"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Code"]
                            ]
                        , div_
                            [ aria_ "selected" "true"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-895417-tab-1"
                            , id_ "tabs-895417-panel-1"
                            , role_ "tabpanel"
                            ]
                            [ div_
                                [ class_
                                    "ring-offset-background focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 relative rounded-md border"
                                ]
                                [ div_
                                    [ class_
                                        "preview flex min-h-[350px] w-full justify-center p-10 items-center"
                                    ]
                                    [ div_
                                        [class_ "w-full max-w-md"]
                                        [ section_
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
                                                            , strokeLinejoin_ "round"
                                                            , strokeLinecap_ "round"
                                                            , strokeWidth_ "2"
                                                            , stroke_ "currentColor"
                                                            , fill_ "none"
                                                            , viewBox_ "0 0 24 24"
                                                            , height_ "24"
                                                            , width_ "24"
                                                            , xmlns_ "http://www.w3.org/2000/svg"
                                                            ]
                                                            [path_ [d_ "m6 9 6 6 6-6"]]
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
                                                            , strokeLinejoin_ "round"
                                                            , strokeLinecap_ "round"
                                                            , strokeWidth_ "2"
                                                            , stroke_ "currentColor"
                                                            , fill_ "none"
                                                            , viewBox_ "0 0 24 24"
                                                            , height_ "24"
                                                            , width_ "24"
                                                            , xmlns_ "http://www.w3.org/2000/svg"
                                                            ]
                                                            [path_ [d_ "m6 9 6 6 6-6"]]
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
                                                            , strokeLinejoin_ "round"
                                                            , strokeLinecap_ "round"
                                                            , strokeWidth_ "2"
                                                            , stroke_ "currentColor"
                                                            , fill_ "none"
                                                            , viewBox_ "0 0 24 24"
                                                            , height_ "24"
                                                            , width_ "24"
                                                            , xmlns_ "http://www.w3.org/2000/svg"
                                                            ]
                                                            [path_ [d_ "m6 9 6 6 6-6"]]
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
                                        ]
                                    ]
                                ]
                            ]
                        , div_
                            [ hidden_ False
                            , aria_ "selected" "false"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-895417-tab-2"
                            , id_ "tabs-895417-panel-2"
                            , role_ "tabpanel"
                            ]
                            [ div_
                                [class_ "relative "]
                                [ pre_
                                    [ class_
                                        "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                                    ]
                                    [ code_
                                        [ data_ "highlighted" "yes"
                                        , class_
                                            "language-html !bg-muted/40 !p-3.5 hljs language-xml"
                                        ]
                                        ["code goes here"]
                                    ]
                                , button_
                                    [ class_
                                        "btn-icon-ghost size-8 absolute right-2.5 top-2 text-muted-foreground hover:text-foreground group"
                                    ]
                                    [ svg_
                                        [ class_ "group-[.copied]:hidden"
                                        , strokeLinejoin_ "round"
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
                                            [ ry_ "2"
                                            , rx_ "2"
                                            , y_ "8"
                                            , x_ "8"
                                            , height_ "14"
                                            , width_ "14"
                                            ]
                                        , path_
                                            [ d_
                                                "M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"
                                            ]

                                    , svg_
                                        [ class_ "hidden group-[.copied]:block"
                                        , strokeLinejoin_ "round"
                                        , strokeLinecap_ "round"
                                        , strokeWidth_ "2"
                                        , stroke_ "currentColor"
                                        , fill_ "none"
                                        , viewBox_ "0 0 24 24"
                                        , height_ "24"
                                        , width_ "24"
                                        , xmlns_ "http://www.w3.org/2000/svg"
                                        ]
                                        [path_ [d_ "M20 6 9 17l-5-5"]]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                , h2_
                    [id_ "usage"]
                    [a_ [href_ "#usage"] ["Usage"]]
                , p_
                    [class_ "prose"]
                    [ "Basecoat already animates"
                    , code_ [] ["<", "details", ">"]
                    , "elements by default. The example add some Tailwind CSS utility classes for style and a bit of vanilla JavaScript to handle the open/close state."
                    ]
                ]
            ]
        , div_
            [ class_
                "hidden text-sm xl:block w-full max-w-[300px]"
            ]
            [ nav_
                [ class_
                    "sticky top-22 space-y-2 [&_ul]:m-0 [&_ul]:list-none [&_ul_ul]:pl-4 [&_li]:mt-0 [&_li]:pt-2 [&_a]:inline-block [&_a]:no-underline [&_a]:transition-colors [&_a]:hover:text-foreground [&_a]:text-muted-foreground"
                ]
                [ h4_ [class_ "font-medium"] ["On This Page"]
                , ul_ [] [li_ [] [a_ [href_ "#usage"] ["Usage"]]]
                ]
            ]
        ]
    ]

