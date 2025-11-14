{-# LANGUAGE OverloadedStrings #-}
module Installation where

import Miso.Property
import Miso.Types
import Miso.Html hiding (data_)
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (id_, height_, width_, path_, target_)

installationPage :: View model action
installationPage =
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
                    ["Installation"]
                , p_
                    [ class_
                        "text-muted-foreground text-[1.05rem] sm:text-base"
                    ]
                    ["How to install and use Basecoat in your app."]
                ]
            , article_
                [class_ "pb-12 mt-8 content"]
                [ h2_
                    [id_ "install-cdn"]
                    [ a_
                        [href_ "#install-cdn"]
                        ["Install using the CDN"]
                    ]
                , h3_
                    [id_ "install-cdn-all"]
                    [ a_
                        [href_ "#install-cdn-all"]
                        ["Install all components"]
                    ]
                , section_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "Add the following to the"
                        , code_ [] ["<", "head", ">"]
                        , "of your HTML file:"
                        ]
                    ]
                , div_
                    [class_ "relative my-4"]
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
                , h3_
                    [id_ "install-cdn-specific"]
                    [ a_
                        [href_ "#install-cdn-specific"]
                        ["Install specific components"]
                    ]
                , section_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "While the JavaScript file for all components is small (around 3kB gzipped), you can cherry-pick the components you need as instructed in the component's page ("
                        , a_
                            [href_ "/components/dropdown-menu"]
                            ["Dropdown Menu"]
                        , ","
                        , a_ [href_ "/components/popover"] ["Popover"]
                        , ","
                        , a_ [href_ "/components/select"] ["Select"]
                        , ","
                        , a_ [href_ "/components/sidebar"] ["Sidebar"]
                        , ","
                        , a_ [href_ "/components/tabs"] ["Tabs"]
                        , "and"
                        , a_ [href_ "/components/toast"] ["Toast"]
                        , "). For example, to add the"
                        , a_
                            [href_ "/components/dropdown-menu"]
                            ["Dropdown Menu"]
                        , "component, add the following to the"
                        , code_ [] ["<", "head", ">"]
                        , "of your HTML file:"
                        ]
                    ]
                , div_
                    [class_ "relative my-4"]
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
                , h2_
                    [id_ "install-npm"]
                    [a_ [href_ "#install-npm"] ["Install using NPM"]]
                , ol_
                    [ class_
                        "[&amp;&gt;h3]:step steps mb-12 md:ml-4 md:border-l md:pl-8"
                    ]
                    [ li_
                        [class_ "step"]
                        [ h3_
                            [ id_ "install-npm-tailwind"
                            , class_
                                "mb-4 scroll-m-20 font-semibold tracking-tight"
                            ]
                            [ a_
                                [href_ "#install-npm-tailwind"]
                                ["Add Tailwind CSS"]
                            ]
                        , div_
                            [class_ "prose"]
                            [ p_
                                []
                                [ "Basecoat uses Tailwind CSS. You need to install it in your project."
                                ]
                            , p_
                                []
                                [ a_
                                    [ target_ "_blank"
                                    , href_ "https://tailwindcss.com/docs/installation"
                                    ]
                                    [ "Follow the Tailwind CSS installation instructions to get started."
                                    ]
                                ]
                            ]
                        ]
                    , li_
                        [class_ "step mt-8"]
                        [ h3_
                            [ id_ "install-npm-basecoat"
                            , class_
                                "mb-4 scroll-m-20 font-semibold tracking-tight"
                            ]
                            [ a_
                                [href_ "#install-npm-basecoat"]
                                ["Add Basecoat"]
                            ]
                        , div_
                            [class_ "prose"]
                            [p_ [] ["Add Basecoat to your Tailwind CSS file."]]
                        , div_
                            [ data_ "tabs-initialized" "true"
                            , id_ "npm-install"
                            , class_ "tabs "
                            ]
                            [ nav_
                                [ aria_ "orientation" "horizontal"
                                , role_ "tablist"
                                ]
                                [ button_
                                    [ tabindex_ "0"
                                    , aria_ "selected" "true"
                                    , aria_ "controls" "npm-install-panel-1"
                                    , id_ "npm-install-tab-1"
                                    , role_ "tab"
                                    , type_ "button"
                                    ]
                                    ["npm"]
                                , button_
                                    [ tabindex_ "0"
                                    , aria_ "selected" "false"
                                    , aria_ "controls" "npm-install-panel-2"
                                    , id_ "npm-install-tab-2"
                                    , role_ "tab"
                                    , type_ "button"
                                    ]
                                    ["pnpm"]
                                , button_
                                    [ tabindex_ "0"
                                    , aria_ "selected" "false"
                                    , aria_ "controls" "npm-install-panel-3"
                                    , id_ "npm-install-tab-3"
                                    , role_ "tab"
                                    , type_ "button"
                                    ]
                                    ["bun"]
                                , button_
                                    [ tabindex_ "0"
                                    , aria_ "selected" "false"
                                    , aria_ "controls" "npm-install-panel-4"
                                    , id_ "npm-install-tab-4"
                                    , role_ "tab"
                                    , type_ "button"
                                    ]
                                    ["yarn"]
                                ]
                            , div_
                                [ aria_ "selected" "true"
                                , tabindex_ "-1"
                                , aria_ "labelledby" "npm-install-tab-1"
                                , id_ "npm-install-panel-1"
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
                                            ["npm install basecoat-css"]
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
                            , div_
                                [ textProp "hidden" ""
                                , aria_ "selected" "false"
                                , tabindex_ "-1"
                                , aria_ "labelledby" "npm-install-tab-2"
                                , id_ "npm-install-panel-2"
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
                                            ["pnpm add basecoat-css"]
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
                            , div_
                                [ textProp "hidden" ""
                                , aria_ "selected" "false"
                                , tabindex_ "-1"
                                , aria_ "labelledby" "npm-install-tab-3"
                                , id_ "npm-install-panel-3"
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
                                            ["bun add basecoat-css"]
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
                            , div_
                                [ textProp "hidden" ""
                                , aria_ "selected" "false"
                                , tabindex_ "-1"
                                , aria_ "labelledby" "npm-install-tab-4"
                                , id_ "npm-install-panel-4"
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
                                            ["yarn add basecoat-css"]
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
                        , div_
                            [class_ "prose"]
                            [ p_
                                []
                                [ "Alternatively, you can directly"
                                , a_
                                    [ target_ "_blank"
                                    , href_
                                        "https://github.com/hunvreus/basecoat/blob/main/src/css/basecoat.css"
                                    ]
                                    ["copy the", code_ [] ["basecoat.css"], "file"]
                                , "into your project."
                                ]
                            ]
                        ]
                    , li_
                        [class_ "step mt-8"]
                        [ h3_
                            [ id_ "install-npm-import"
                            , class_
                                "mb-4 scroll-m-20 font-semibold tracking-tight"
                            ]
                            [ a_
                                [href_ "#install-npm-import"]
                                ["Import basecoat in your CSS"]
                            ]
                        , div_
                            [class_ "relative my-4"]
                            [ pre_
                                [ class_
                                    "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                                ]
                                [ code_
                                    [ data_ "highlighted" "yes"
                                    , class_ "language-css !bg-muted/40 !p-3.5 hljs"
                                    ]
                                    [ "code goes here"
                                    ]
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
                    , li_
                        [class_ "step mt-8"]
                        [ h3_
                            [ id_ "install-npm-js"
                            , class_
                                "mb-4 scroll-m-20 font-semibold tracking-tight"
                            ]
                            [ a_
                                [href_ "#install-npm-js"]
                                ["(Optional) Add JavaScript files"]
                            ]
                        , div_
                            [class_ "prose"]
                            [ p_
                                []
                                [ "Some components"
                                , a_ [href_ "#install-js"] ["need some JavaScript"]
                                , "(e.g. the"
                                , a_ [href_ "/components/tabs"] ["Tabs"]
                                , "component). There are two ways to get it into your project:"
                                ]
                            , h4_
                                [class_ "mt-6 font-semibold"]
                                ["Using a build tool (Vite, Webpack…)"]
                            , p_
                                []
                                [ "If your project is ESM-aware you can directly import the scripts. To include all components:"
                                ]
                            ]
                        , div_
                            [class_ "relative my-4"]
                            [ pre_
                                [ class_
                                    "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                                ]
                                [ code_
                                    [ data_ "highlighted" "yes"
                                    , class_
                                        "language-js !bg-muted/40 !p-3.5 hljs language-javascript"
                                    ]
                                    [ "code goes here"
                                    ]
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
                        , div_
                            [class_ "prose"]
                            [ p_
                                []
                                [ "Or cherry-pick specific components, for example:"
                                ]
                            ]
                        , div_
                            [class_ "relative my-4"]
                            [ pre_
                                [ class_
                                    "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                                ]
                                [ code_
                                    [ data_ "highlighted" "yes"
                                    , class_
                                        "language-js !bg-muted/40 !p-3.5 hljs language-javascript"
                                    ]
                                    [ "code goes here"
                                    ]
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
                        , div_
                            [class_ "prose"]
                            [ h4_
                                [class_ "mt-6 font-semibold"]
                                ["Without a build tool"]
                            , p_
                                []
                                [ "Copy the pre-built files from"
                                , code_ [] ["node_modules"]
                                , "into your public directory:"
                                ]
                            ]
                        , div_
                            [class_ "relative my-4"]
                            [ pre_
                                [ class_
                                    "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                                ]
                                [ code_
                                    [ data_ "highlighted" "yes"
                                    , class_ "language-bash !bg-muted/40 !p-3.5 hljs"
                                    ]
                                    [ "code goes here"
                                    ]
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
                        , div_
                            [class_ "prose"]
                            [ p_
                                []
                                [ "Then reference the script you need as well as the"
                                , code_ [] ["basecoat.min.js"]
                                , "file (used to register and initialize components):"
                                ]
                            ]
                        , div_
                            [class_ "relative my-4"]
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
                        , div_
                            [class_ "prose"]
                            [ p_
                                []
                                [ "See each component page for the minimal script required."
                                ]
                            ]
                        ]
                    , li_
                        [class_ "step mt-8"]
                        [ h3_
                            [ id_ "install-npm-done"
                            , class_
                                "mb-4 scroll-m-20 font-semibold tracking-tight"
                            ]
                            [a_ [href_ "#install-npm-done"] ["That's it"]]
                        , div_
                            [class_ "prose"]
                            [ p_
                                []
                                [ "You can now use any of the Basecoat classes in your project (e.g."
                                , code_ [] ["btn"]
                                , ","
                                , code_ [] ["card"]
                                , ","
                                , code_ [] ["input"]
                                , ", etc). Read the documentation about each component to get started (e.g."
                                , a_ [href_ "/components/button"] ["button"]
                                , ","
                                , a_ [href_ "/components/card"] ["card"]
                                , ","
                                , a_ [href_ "/components/input"] ["input"]
                                , ", etc)."
                                ]
                            , p_
                                []
                                [ "Some components (e.g."
                                , a_ [href_ "/components/select"] ["Select"]
                                , ") require"
                                , a_
                                    [href_ "#install-js"]
                                    ["JavaScript code to work"]
                                , "."
                                ]
                            ]
                        ]
                    ]
                , h2_
                    [id_ "install-js"]
                    [ a_
                        [href_ "#install-js"]
                        ["Components with JavaScript"]
                    ]
                , div_
                    [class_ "prose"]
                    [ p_
                        []
                        [ b_
                            []
                            [ "A handful of components require JavaScript code to work."
                            ]
                        , ", specifically"
                        , a_
                            [href_ "/components/dropdown-menu"]
                            ["Dropdown Menu"]
                        , ","
                        , a_ [href_ "/components/popover"] ["Popover"]
                        , ","
                        , a_ [href_ "/components/select"] ["Select"]
                        , ","
                        , a_ [href_ "/components/sidebar"] ["Sidebar"]
                        , ","
                        , a_ [href_ "/components/tabs"] ["Tabs"]
                        , "and"
                        , a_ [href_ "/components/toast"] ["Toast"]
                        ]
                    , p_
                        []
                        [ "If a component requires JavaScript, the documentation page will provide instructions. There are 2 options to add the JavaScript code to your project:"
                        ]
                    , ul_
                        []
                        [ li_
                            []
                            [ b_ [] ["CDN"]
                            , ": you either"
                            , a_
                                [href_ "#install-cdn-all"]
                                ["add the code for all of the components"]
                            , "to the"
                            , code_ [] ["<", "head", ">"]
                            , "of your HTML file, or just the file for the component you want to use as instructed on the component's page."
                            ]
                        , li_
                            []
                            [ b_ [] ["Local file"]
                            , ": you download the script as a separate file and include it in your project ("
                            , a_
                                [href_ "#install-cli"]
                                ["or let the CLI do it for you"]
                            , "). Once again, you have the choice to include the file for all components at once ("
                            , a_
                                []
                                [ code_ [] ["all.min.js"]
                                , "or"
                                , code_ [] ["all.js"]
                                , ")."
                                , p_ [] []
                                ]
                            ]
                        ]
                    , a_ [] []
                    ]
                , a_ [] []
                , h2_
                    [id_ "install-cli"]
                    [ a_ [] []
                    , a_ [href_ "#install-cli"] ["Use the CLI"]
                    ]
                , div_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "If you're using Nunjucks or Jinja, you can use the CLI to install the CSS and macros for any of the complex components(e.g."
                        , a_ [href_ "/components/dialog"] ["Dialog"]
                        , "). It will copy the JS and"
                        , code_ [] [".html.jinja"]
                        , "or"
                        , code_ [] [".njk"]
                        , "macros into your code."
                        ]
                    , p_
                        []
                        [ "For example, to add the Dialog component, run one of the following commands:"
                        ]
                    ]
                , div_
                    [ data_ "tabs-initialized" "true"
                    , id_ "cli-execution"
                    , class_ "tabs "
                    ]
                    [ nav_
                        [ aria_ "orientation" "horizontal"
                        , role_ "tablist"
                        ]
                        [ button_
                            [ tabindex_ "0"
                            , aria_ "selected" "true"
                            , aria_ "controls" "cli-execution-panel-1"
                            , id_ "cli-execution-tab-1"
                            , role_ "tab"
                            , type_ "button"
                            ]
                            ["npm"]
                        , button_
                            [ tabindex_ "0"
                            , aria_ "selected" "false"
                            , aria_ "controls" "cli-execution-panel-2"
                            , id_ "cli-execution-tab-2"
                            , role_ "tab"
                            , type_ "button"
                            ]
                            ["pnpm"]
                        , button_
                            [ tabindex_ "0"
                            , aria_ "selected" "false"
                            , aria_ "controls" "cli-execution-panel-3"
                            , id_ "cli-execution-tab-3"
                            , role_ "tab"
                            , type_ "button"
                            ]
                            ["bun"]
                        , button_
                            [ tabindex_ "0"
                            , aria_ "selected" "false"
                            , aria_ "controls" "cli-execution-panel-4"
                            , id_ "cli-execution-tab-4"
                            , role_ "tab"
                            , type_ "button"
                            ]
                            ["yarn"]
                        ]
                    , div_
                        [ aria_ "selected" "true"
                        , tabindex_ "-1"
                        , aria_ "labelledby" "cli-execution-tab-1"
                        , id_ "cli-execution-panel-1"
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
                                    ["npx basecoat-cli add dialog"]
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
                    , div_
                        [ textProp "hidden" ""
                        , aria_ "selected" "false"
                        , tabindex_ "-1"
                        , aria_ "labelledby" "cli-execution-tab-2"
                        , id_ "cli-execution-panel-2"
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
                                    ["pnpm dlx basecoat-cli add dialog"]
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
                    , div_
                        [ textProp "hidden" ""
                        , aria_ "selected" "false"
                        , tabindex_ "-1"
                        , aria_ "labelledby" "cli-execution-tab-3"
                        , id_ "cli-execution-panel-3"
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
                                    ["bunx basecoat-cli add dialog"]
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
                    , div_
                        [ textProp "hidden" ""
                        , aria_ "selected" "false"
                        , tabindex_ "-1"
                        , aria_ "labelledby" "cli-execution-tab-4"
                        , id_ "cli-execution-panel-4"
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
                                    ["yarn dlx basecoat-cli add dialog"]
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
                , div_
                    [class_ "prose my-6"]
                    [ p_
                        []
                        [ "You will be asked to confirm the template engine and destination directory for the JavaScript and macros."
                        ]
                    ]
                , h2_
                    [id_ "install-macros"]
                    [ a_
                        [href_ "#install-macros"]
                        ["Use Nunjucks or Jinja macros"]
                    ]
                , div_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "For more complex components, you can use"
                        , a_
                            [ target_ "_blank"
                            , href_ "https://mozilla.github.io/nunjucks/"
                            ]
                            ["Nunjucks"]
                        , "or"
                        , a_
                            [ target_ "_blank"
                            , href_ "https://jinja.palletsprojects.com"
                            ]
                            ["Jinja"]
                        , "macros to help you generate the HTML and JavaScript code."
                        ]
                    , p_
                        []
                        [ "To install them, either"
                        , a_
                            [ target_ "_blank"
                            , href_
                                "https://github.com/hunvreus/basecoat/tree/main/src"
                            ]
                            ["copy them directly from the GitHub repository"]
                        , "or use"
                        , a_ [href_ "#install-cli"] ["the CLI"]
                        , "to do the work for you."
                        ]
                    , p_
                        []
                        [ "For example, here's how you could use the"
                        , code_ [] ["select()"]
                        , "macro to generate the HTML and JavaScript code for a"
                        , a_ [href_ "/components/select"] ["Select"]
                        , "component:"
                        ]
                    ]
                , div_
                    [class_ "relative my-4"]
                    [ pre_
                        [ class_
                            "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                        ]
                        [ code_
                            [ data_ "highlighted" "yes"
                            , class_
                                "language-jinja !bg-muted/40 !p-3.5 hljs language-django"
                            ]
                            ["foo"]
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
                , h2_
                    [id_ "install-icons"]
                    [a_ [href_ "#install-icons"] ["Icons"]]
                , div_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "Basecoat uses"
                        , a_
                            [target_ "_blank", href_ "https://lucide.dev"]
                            ["Lucide icons"]
                        , ". You have three options:"
                        ]
                    , ul_
                        []
                        [ li_
                            []
                            [ b_ [] ["Copy SVG code"]
                            , ": Visit"
                            , a_
                                [ target_ "_blank"
                                , href_ "https://lucide.dev/icons"
                                ]
                                ["lucide.dev/icons"]
                            , ", click any icon to copy the SVG, and paste it directly in your HTML. Best for simple projects or occasional icon usage."
                            ]
                        , li_
                            []
                            [ b_ [] ["Install lucide"]
                            , ": Follow the"
                            , a_
                                [ target_ "_blank"
                                , href_ "https://lucide.dev/guide/installation"
                                ]
                                ["installation guide"]
                            , "to install the"
                            , code_ [] ["lucide"]
                            , "package and dynamically render icons. Best for projects that need many icons or want tree-shaking."
                            ]
                        , li_
                            []
                            [ b_ [] ["Use framework packages"]
                            , ": Browse"
                            , a_
                                [ target_ "_blank"
                                , href_ "https://lucide.dev/packages"
                                ]
                                ["framework-specific packages"]
                            , "(React, Vue, Angular, etc.) for component-based icon usage. Best if you're using a supported framework."
                            ]
                        ]
                    ]
                , div_
                    [class_ "flex flex-wrap gap-2 my-6"]
                    [ a_
                        [ target_ "_blank"
                        , href_ "https://lucide.dev"
                        , class_ "badge-outline"
                        ]
                        [ "Browse icons"
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
                            [ path_ [d_ "M5 12h14"]
                            , path_ [d_ "m12 5 7 7-7 7"]
                            ]
                        ]
                    , a_
                        [ target_ "_blank"
                        , href_ "https://lucide.dev/guide/installation"
                        , class_ "badge-outline"
                        ]
                        [ "Installation guide"
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
                            [ path_ [d_ "M5 12h14"]
                            , path_ [d_ "m12 5 7 7-7 7"]
                            ]
                        ]
                    , a_
                        [ target_ "_blank"
                        , href_ "https://lucide.dev/packages"
                        , class_ "badge-outline"
                        ]
                        [ "Packages"
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
                            [ path_ [d_ "M5 12h14"]
                            , path_ [d_ "m12 5 7 7-7 7"]
                            ]
                        ]
                    ]
                , h2_
                    [id_ "install-theming"]
                    [a_ [href_ "#install-theming"] ["Theming"]]
                , div_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "You can import any"
                        , a_
                            [ target_ "_blank"
                            , href_ "https://shadcn-ui.com"
                            ]
                            ["shadcn/ui"]
                        , "compatible theme (e.g."
                        , a_
                            [target_ "_blank", href_ "https://tweakcn.com"]
                            ["tweakcn"]
                        , "). For example:"
                        ]
                    , ul_
                        []
                        [ li_
                            []
                            [ "Go to"
                            , a_
                                [ target_ "_blank"
                                , href_ "https://ui.shadcn.com/themes"
                                ]
                                ["ui.shadcn.com/themes"]
                            , "and select a theme."
                            ]
                        , li_
                            []
                            [ "Click Copy code and save the theme variables in a file (e.g. theme.css)."
                            ]
                        , li_
                            []
                            [ "Import the theme in your CSS file:"
                            , div_
                                [class_ "relative my-4"]
                                [ pre_
                                    [ class_
                                        "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                                    ]
                                    [ code_
                                        [ data_ "highlighted" "yes"
                                        , class_ "language-css !bg-muted/40 !p-3.5 hljs"
                                        ]
                                        [ "code goes here"
                                        ]
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
                , div_
                    [class_ "flex flex-wrap gap-2 my-6"]
                    [ a_
                        [ target_ "_blank"
                        , href_ "https://ui.shadcn.com/docs/theming"
                        , class_ "badge-outline"
                        ]
                        [ "About shadcn/ui theming"
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
                            [ path_ [d_ "M5 12h14"]
                            , path_ [d_ "m12 5 7 7-7 7"]
                            ]
                        ]
                    ]
                , h2_
                    [id_ "install-customization"]
                    [a_ [href_ "#install-manual"] ["Customization"]]
                , div_
                    [class_ "prose"]
                    [ section_
                        [class_ "prose"]
                        [ p_
                            []
                            [ "You can override default styles using Tailwind:"
                            ]
                        ]
                    , div_
                        [class_ "relative my-4"]
                        [ pre_
                            [ class_
                                "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                            ]
                            [ code_
                                [ data_ "highlighted" "yes"
                                , class_
                                    "language-html !bg-muted/40 !p-3.5 hljs language-xml"
                                ]
                                [ "code goes here"
                                ]
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
                    , section_
                        [class_ "prose"]
                        [ p_
                            []
                            [ "You can also extend or override the existing styles in your own Tailwind files:"
                            ]
                        ]
                    , div_
                        [class_ "relative my-4"]
                        [ pre_
                            [ class_
                                "grid text-sm max-h-[650px] overflow-y-auto rounded-xl scrollbar"
                            ]
                            [ code_
                                [ data_ "highlighted" "yes"
                                , class_ "language-css !bg-muted/40 !p-3.5 hljs"
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
                    , section_
                        [class_ "prose"]
                        [ p_
                            []
                            [ "More importantly, you can use the"
                            , a_ [href_ "#install-theming"] ["theme variables"]
                            , "to customize many aspects of the components (colors, fonts, sizes, etc)."
                            ]
                        , p_
                            []
                            [ b_ [] ["If you want to make more drastic changes"]
                            , ", you can"
                            , a_
                                [ target_ "_blank"
                                , href_
                                    "https://github.com/hunvreus/basecoat/blob/main/src/css/basecoat.css"
                                ]
                                ["copy the", code_ [] ["basecoat.css"], "file"]
                            , "into your project and make your changes there."
                            ]
                        ]
                    ]
                ]
            ]
        , div_
            [ class_
                "hidden text-sm xl:block w-full max-w-[300px]"
            ]
            [ nav_
                [ class_
                    "sticky top-22 space-y-2 [&amp;_ul]:m-0 [&amp;_ul]:list-none [&amp;_ul_ul]:pl-4 [&amp;_li]:mt-0 [&amp;_li]:pt-2 [&amp;_a]:inline-block [&amp;_a]:no-underline [&amp;_a]:transition-colors [&amp;_a]:hover:text-foreground [&amp;_a]:text-muted-foreground"
                ]
                [ h4_ [class_ "font-medium"] ["On This Page"]
                , ul_
                    []
                    [ li_
                        []
                        [ a_
                            [href_ "#install-cdn"]
                            ["Install using the CDN"]
                        , ul_
                            []
                            [ li_
                                []
                                [ a_
                                    [href_ "#install-cdn-all"]
                                    ["Install all components"]
                                ]
                            , li_
                                []
                                [ a_
                                    [href_ "#install-cdn-specific"]
                                    ["Install specific components"]
                                ]
                            ]
                        ]
                    , li_
                        []
                        [ a_
                            [href_ "#install-npm"]
                            ["Installing using NPM"]
                        , ul_
                            []
                            [ li_
                                []
                                [ a_
                                    [href_ "#install-npm-tailwind"]
                                    ["Add Tailwind CSS"]
                                ]
                            , li_
                                []
                                [ a_
                                    [href_ "#install-npm-basecoat"]
                                    ["Add Basecoat"]
                                ]
                            , li_
                                []
                                [ a_
                                    [href_ "#install-npm-import"]
                                    ["Import basecoat in your CSS"]
                                ]
                            , li_
                                []
                                [ a_
                                    [href_ "#install-npm-js"]
                                    ["(Optional) Add JavaScript files"]
                                ]
                            , li_
                                []
                                [a_ [href_ "#install-npm-done"] ["That's it"]]
                            ]
                        ]
                    , li_
                        []
                        [ a_
                            [href_ "#install-js"]
                            ["Components with JavaScript"]
                        ]
                    , li_ [] [a_ [href_ "#install-cli"] ["Use the CLI"]]
                    , li_
                        []
                        [ a_
                            [href_ "#install-macros"]
                            ["Use Nunjucks or Jinja macros"]
                        ]
                    , li_ [] [a_ [href_ "#install-icons"] ["Icons"]]
                    , li_ [] [a_ [href_ "#install-theming"] ["Theming"]]
                    , li_
                        []
                        [ a_
                            [href_ "#install-customization"]
                            ["Customization"]
                        ]
                    ]
                ]
            ]
        ]
    ]


