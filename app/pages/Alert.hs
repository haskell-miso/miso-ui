{-# LANGUAGE OverloadedStrings #-}
module Alert where

import Miso.Types
import Miso.Html hiding (data_)
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (id_, height_, width_, path_)

alertPage :: View model action
alertPage = div_
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
                    ["Alert"]
                , p_
                    [ class_
                        "text-muted-foreground text-[1.05rem] sm:text-base"
                    ]
                    ["Displays a callout for user attention."]
                ]
            , article_
                [class_ "pb-12 mt-8 content"]
                [ div_
                    [class_ "relative my-6"]
                    [ div_
                        [ data_ "tabs-initialized" "true"
                        , id_ "tabs-471759"
                        , class_ "tabs "
                        ]
                        [ nav_
                            [ aria_ "orientation" "horizontal"
                            , role_ "tablist"
                            ]
                            [ button_
                                [ tabindex_ "-1"
                                , aria_ "selected" "false"
                                , aria_ "controls" "tabs-471759-panel-1"
                                , id_ "tabs-471759-tab-1"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Preview"]
                            , button_
                                [ tabindex_ "0"
                                , aria_ "selected" "true"
                                , aria_ "controls" "tabs-471759-panel-2"
                                , id_ "tabs-471759-tab-2"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Code"]
                            ]
                        , div_
                            [ hidden_ False
                            , aria_ "selected" "true"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-471759-tab-1"
                            , id_ "tabs-471759-panel-1"
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
                                        []
                                        [ div_
                                            [ class_ "grid w-full max-w-xl items-start gap-4"
                                            ]
                                            [ div_
                                                [class_ "alert"]
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
                                                    , path_ [d_ "m9 12 2 2 4-4"]
                                                    ]
                                                , h2_ [] ["Success! Your changes have been saved"]
                                                , section_
                                                    []
                                                    [ "This is an alert with icon, title and description."
                                                    ]
                                                ]
                                            , div_
                                                [class_ "alert"]
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
                                                    [ path_
                                                        [ d_
                                                            "M18 8a2 2 0 0 0 0-4 2 2 0 0 0-4 0 2 2 0 0 0-4 0 2 2 0 0 0-4 0 2 2 0 0 0 0 4"
                                                        ]
                                                    , path_ [d_ "M10 22 9 8"]
                                                    , path_ [d_ "m14 22 1-14"]
                                                    , path_
                                                        [ d_
                                                            "M20 8c.5 0 .9.4.8 1l-2.6 12c-.1.5-.7 1-1.2 1H7c-.6 0-1.1-.4-1.2-1L3.2 9c-.1-.6.3-1 .8-1Z"
                                                        ]
                                                    ]
                                                , h2_
                                                    []
                                                    [ "This Alert has a title and an icon. No description."
                                                    ]
                                                , section_
                                                    []
                                                    [ "This is an alert with icon, title and description."
                                                    ]
                                                ]
                                            , div_
                                                [class_ "alert-destructive"]
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
                                                    , path_ [d_ "m9 12 2 2 4-4"]
                                                    ]
                                                , h2_ [] ["Unable to process your payment."]
                                                , section_
                                                    []
                                                    [ p_
                                                        []
                                                        [ "Please verify your billing information and try again."
                                                        ]
                                                    , ul_
                                                        [class_ "list-inside list-disc text-sm"]
                                                        [ li_ [] ["Check your card details"]
                                                        , li_ [] ["Ensure sufficient funds"]
                                                        , li_ [] ["Verify billing address"]
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div_
                            [ aria_ "selected" "false"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-471759-tab-2"
                            , id_ "tabs-471759-panel-2"
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
                                        [ "code goes here" ]
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
                , h2_ [id_ "usage"] ["Usage"]
                , section_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "Use the"
                        , code_ [] ["alert"]
                        , "or"
                        , code_ [] ["alert-destructive"]
                        , "."
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
                        [ "The component has the following HTML structure:"
                        ]
                    , dl_
                        []
                        [ dt_
                            []
                            [ code_
                                [ data_ "highlighted" "yes"
                                , class_ "highlight language-html hljs language-xml"
                                ]
                                [ "code goes here"
                                ]
                            ]
                        , dd_
                            []
                            [ "Main container. Use"
                            , code_ [] ["alert"]
                            , "for default styling or"
                            , code_ [] ["alert-destructive"]
                            , "for error states."
                            , dl_
                                []
                                [ dt_
                                    []
                                    [ code_
                                        [ data_ "highlighted" "yes"
                                        , class_ "highlight language-html hljs language-xml"
                                        ]
                                        [ span_
                                            [class_ "hljs-tag"]
                                            ["<", span_ [class_ "hljs-name"] ["svg"], ">"]
                                        ]
                                    , span_ [class_ "badge-secondary"] ["Optional"]
                                    ]
                                , dd_ [] ["The icon."]
                                , dt_
                                    []
                                    [ code_
                                        [ data_ "highlighted" "yes"
                                        , class_ "highlight language-html hljs language-xml"
                                        ]
                                        [ span_
                                            [class_ "hljs-tag"]
                                            ["<", span_ [class_ "hljs-name"] ["h2"], ">"]
                                        ]
                                    ]
                                , dd_ [] ["The title."]
                                , dt_
                                    []
                                    [ code_
                                        [ data_ "highlighted" "yes"
                                        , class_ "highlight language-html hljs language-xml"
                                        ]
                                        [ span_
                                            [class_ "hljs-tag"]
                                            ["<", span_ [class_ "hljs-name"] ["section"], ">"]
                                        ]
                                    , span_ [class_ "badge-secondary"] ["Optional"]
                                    ]
                                , dd_ [] ["The description."]
                                ]
                            ]
                        ]
                    ]
                , h2_
                    [id_ "examples"]
                    [a_ [href_ "#examples"] ["Examples"]]
                , h3_
                    [id_ "example-destructive"]
                    [ a_ [href_ "#example-destructive"] ["Destructive"]
                    ]
                , div_
                    [class_ "relative my-6"]
                    [ div_
                        [ data_ "tabs-initialized" "true"
                        , id_ "tabs-822018"
                        , class_ "tabs "
                        ]
                        [ nav_
                            [ aria_ "orientation" "horizontal"
                            , role_ "tablist"
                            ]
                            [ button_
                                [ tabindex_ "0"
                                , aria_ "selected" "true"
                                , aria_ "controls" "tabs-822018-panel-1"
                                , id_ "tabs-822018-tab-1"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Preview"]
                            , button_
                                [ tabindex_ "0"
                                , aria_ "selected" "false"
                                , aria_ "controls" "tabs-822018-panel-2"
                                , id_ "tabs-822018-tab-2"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Code"]
                            ]
                        , div_
                            [ aria_ "selected" "true"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-822018-tab-1"
                            , id_ "tabs-822018-panel-1"
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
                                        []
                                        [ div_
                                            [class_ "alert-destructive"]
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
                                            , h2_ [] ["Something went wrong!"]
                                            , section_
                                                []
                                                ["Your session has expired. Please log in again."]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div_
                            [ hidden_ False
                            , aria_ "selected" "false"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-822018-tab-2"
                            , id_ "tabs-822018-panel-2"
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
                , h3_
                    [id_ "example-no-description"]
                    [ a_
                        [href_ "#example-no-description"]
                        ["No description"]
                    ]
                , div_
                    [class_ "relative my-6"]
                    [ div_
                        [ data_ "tabs-initialized" "true"
                        , id_ "tabs-755020"
                        , class_ "tabs "
                        ]
                        [ nav_
                            [ aria_ "orientation" "horizontal"
                            , role_ "tablist"
                            ]
                            [ button_
                                [ tabindex_ "0"
                                , aria_ "selected" "true"
                                , aria_ "controls" "tabs-755020-panel-1"
                                , id_ "tabs-755020-tab-1"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Preview"]
                            , button_
                                [ tabindex_ "0"
                                , aria_ "selected" "false"
                                , aria_ "controls" "tabs-755020-panel-2"
                                , id_ "tabs-755020-tab-2"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Code"]
                            ]
                        , div_
                            [ aria_ "selected" "true"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-755020-tab-1"
                            , id_ "tabs-755020-panel-1"
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
                                        []
                                        [ div_
                                            [class_ "alert"]
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
                                                [ path_
                                                    [ d_
                                                        "M20 13c0 5-3.5 7.5-7.66 8.95a1 1 0 0 1-.67-.01C7.5 20.5 4 18 4 13V6a1 1 0 0 1 1-1c2 0 4.5-1.2 6.24-2.72a1.17 1.17 0 0 1 1.52 0C14.51 3.81 17 5 19 5a1 1 0 0 1 1 1z"
                                                    ]
                                                , path_ [d_ "M12 8v4"]
                                                , path_ [d_ "M12 16h.01"]
                                                ]
                                            , h2_
                                                []
                                                [ "This is a very long alert title that demonstrates how the component handles extended text content and potentially wraps across multiple lines"
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
                            , aria_ "labelledby" "tabs-755020-tab-2"
                            , id_ "tabs-755020-panel-2"
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
                , h3_
                    [id_ "example-no-icon"]
                    [a_ [href_ "#example-no-icon"] ["No icon"]]
                , div_
                    [class_ "relative my-6"]
                    [ div_
                        [ data_ "tabs-initialized" "true"
                        , id_ "tabs-599898"
                        , class_ "tabs "
                        ]
                        [ nav_
                            [ aria_ "orientation" "horizontal"
                            , role_ "tablist"
                            ]
                            [ button_
                                [ tabindex_ "0"
                                , aria_ "selected" "true"
                                , aria_ "controls" "tabs-599898-panel-1"
                                , id_ "tabs-599898-tab-1"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Preview"]
                            , button_
                                [ tabindex_ "0"
                                , aria_ "selected" "false"
                                , aria_ "controls" "tabs-599898-panel-2"
                                , id_ "tabs-599898-tab-2"
                                , role_ "tab"
                                , type_ "button"
                                ]
                                ["Code"]
                            ]
                        , div_
                            [ aria_ "selected" "true"
                            , tabindex_ "-1"
                            , aria_ "labelledby" "tabs-599898-tab-1"
                            , id_ "tabs-599898-panel-1"
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
                                        []
                                        [ div_
                                            [class_ "alert"]
                                            [ h2_ [] ["Success! Your changes have been saved"]
                                            , section_
                                                []
                                                [ "This is an alert with icon, title and description."
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
                            , aria_ "labelledby" "tabs-599898-tab-2"
                            , id_ "tabs-599898-panel-2"
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
                                        [ span_
                                            [class_ "hljs-tag"]
                                            [ "<"
                                            , span_ [class_ "hljs-name"] ["div"]
                                            , span_ [class_ "hljs-attr"] ["class"]
                                            , "="
                                            , span_ [class_ "hljs-string"] [" alert "]
                                            , ">"
                                            ]
                                        , span_
                                            [class_ "hljs-tag"]
                                            ["<", span_ [class_ "hljs-name"] ["h2"], ">"]
                                        , "Success! Your changes have been saved"
                                        , span_
                                            [class_ "hljs-tag"]
                                            ["<", "/", span_ [class_ "hljs-name"] ["h2"], ">"]
                                        , span_
                                            [class_ "hljs-tag"]
                                            ["<", span_ [class_ "hljs-name"] ["section"], ">"]
                                        , "This is an alert with icon, title and description."
                                        , span_
                                            [class_ "hljs-tag"]
                                            [ "<"
                                            , "/"
                                            , span_ [class_ "hljs-name"] ["section"]
                                            , ">"
                                            ]
                                        , span_
                                            [class_ "hljs-tag"]
                                            ["<", "/", span_ [class_ "hljs-name"] ["div"], ">"]
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
                    [ li_ [] [a_ [href_ "#usage"] ["Usage"]]
                    , li_
                        []
                        [ a_ [href_ "#examples"] ["Examples"]
                        , ul_
                            []
                            [ li_
                                []
                                [ a_ [href_ "#example-destructive"] ["Destructive"]
                                ]
                            , li_
                                []
                                [ a_
                                    [href_ "#example-no-description"]
                                    ["No description"]
                                ]
                            , li_ [] [a_ [href_ "#example-no-icon"] ["No icon"]]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

