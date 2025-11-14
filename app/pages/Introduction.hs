{-# LANGUAGE OverloadedStrings #-}
module Introduction where

import Miso.Types
import Miso.Html
import Miso.Html.Property

introPage :: View model action
introPage = div_
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
                    ["Introduction"]
                , p_
                    [ class_
                        "text-muted-foreground text-[1.05rem] sm:text-base"
                    ]
                    [ "Basecoat is a set of components built with Tailwind CSS. It is designed to be used with any traditional web stack"
                    ]
                ]
            , article_
                [class_ "pb-12 mt-8 content"]
                [ h2_
                    [id_ "why"]
                    [a_ [href_ "#why"] ["Why Basecoat?"]]
                , div_
                    [class_ "prose"]
                    [ p_
                        []
                        [ b_
                            []
                            [ "Basecoat brings the magic of"
                            , a_ [href_ "https://ui.shadcn.com/"] [" shadcn/ui "]
                            , " to any traditional web stack: no React required."
                            ]
                        ]
                    , p_
                        []
                        [ "Tailwind won. But building UIs with utility classes alone kinda sucks. Most Tailwind libraries like Flowbite, Preline, or even Tailwind UI ask you to copy walls of unreadable classes into your HTML. It works, but it's messy and hard to maintain."
                        ]
                    , p_
                        []
                        [ "shadcn/ui avoids that by wrapping everything in React components. It also gives you a killer design system, theme support, a CLI, and a growing ecosystem."
                        ]
                    , p_
                        []
                        [ "But maybe you're not using React. Maybe your app is built with plain HTML. Or Flask. Or Rails. Or Laravel. Or Django. Or whatever."
                        ]
                    , p_
                        []
                        [ b_ [] ["That's where Basecoat comes in."]
                        , "It gives you modern, accessible components with the simplicity of plain HTML and Tailwind. Basecoat is:"
                        ]
                    , ul_
                        []
                        [ li_
                            []
                            [ b_ [] ["Lightweight"]
                            , ": no runtime JS, just CSS and a tiny bit of vanilla JavaScript for the more interactive components."
                            ]
                        , li_
                            []
                            [ b_ [] ["Easy to use"]
                            , ": add classes like"
                            , code_ [] ["btn"]
                            , "or"
                            , code_ [] ["input"]
                            , "and you're done."
                            ]
                        , li_
                            []
                            [ b_ [] ["Framework-agnostic"]
                            , ": works with any backend or frontend stack."
                            ]
                        , li_
                            []
                            [ b_ [] ["Accessible"]
                            , ": components follow accessibility best practices."
                            ]
                        , li_
                            []
                            [ b_ [] ["Dark mode ready"]
                            , ": respects your Tailwind config."
                            ]
                        , li_
                            []
                            [ b_ [] ["Extendable"]
                            , ": tweak styles with Tailwind or CSS variables."
                            ]
                        , li_
                            []
                            [ b_ [] ["Themable"]
                            , ": fully compatible with shadcn/ui themes."
                            ]
                        , li_
                            []
                            [ b_ [] ["Readable"]
                            , ": no class soup, just clean markup."
                            ]
                        , li_
                            []
                            [b_ [] ["Free and open source"], ": MIT licensed."]
                        ]
                    ]
                , h2_
                    [id_ "how-it-works"]
                    [a_ [href_ "#how-it-works"] ["How it works"]]
                , div_
                    [class_ "prose"]
                    [ p_
                        []
                        [ "Add a single CSS file to your Tailwind setup. Use components by dropping in simple classes like "
                        , code_ [] ["btn"]
                        , ", "
                        , code_ [] ["form"]
                        , ", or"
                        , code_ [] ["card"]
                        , "."
                        ]
                    , p_
                        []
                        [ "Some components (like modals or dropdowns) use a tiny bit of vanilla JavaScript. You can skip it if you don't need interactivity."
                        ]
                    , p_
                        []
                        [ "There's also a CLI to help you scaffold components and get set up fast."
                        ]
                    , p_
                        []
                        [ a_
                            [href_ "/installation"]
                            ["Check out the installation guide"]
                        , "to get started."
                        ]
                    ]
                , h2_
                    [id_ "how-can-i-help"]
                    [ a_ [href_ "#how-can-i-help"] ["How can I help?"]
                    ]
                , div_
                    [class_ "prose"]
                    [ p_ [] ["Basecoat is 100% open source and free."]
                    , ul_
                        []
                        [ li_
                            []
                            [ a_
                                [href_ "https://github.com/hunvreus/basecoat"]
                                ["Star it on GitHub"]
                            ]
                        , li_
                            []
                            [ a_
                                [ href_
                                    "https://github.com/hunvreus/basecoat/issues"
                                ]
                                ["Report bugs or request features"]
                            ]
                        , li_
                            []
                            [ a_
                                [ href_ "https://github.com/hunvreus/basecoat/pulls"
                                ]
                                ["Submit a pull request"]
                            ]
                        , li_
                            []
                            [ a_
                                [href_ "https://github.com/sponsors/hunvreus"]
                                ["Sponsor the project"]
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
                    [ li_ [] [a_ [href_ "#why"] ["Why Basecoat?"]]
                    , li_
                        []
                        [a_ [href_ "#how-it-works"] ["How it works"]]
                    , li_
                        []
                        [ a_ [href_ "#how-can-i-help"] ["How can I help?"]
                        ]
                    ]
                ]
            ]
        ]
    ]
