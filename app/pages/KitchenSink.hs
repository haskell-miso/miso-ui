{-# LANGUAGE OverloadedStrings #-}
module KitchenSink where

import           Miso
import qualified Miso.CSS as CSS
import           Miso.Html hiding (data_)
import qualified Miso.Html.Property as P
import           Miso.Html.Property hiding (max_, min_, label_, form_)
import           Miso.Svg.Element
import           Miso.Svg.Property hiding (id_, height_, width_, path_)

focusable_ :: MisoString -> Attribute action
focusable_ = textProp "focusable"

kitchenSinkPage :: View model action
kitchenSinkPage =
 div_
    [class_ "p-4 md:p-6 xl:p-12"]
    [ div_
        [class_ "flex gap-x-10 justify-center"]
        [ div_
            []
            [ header_
                [class_ "space-y-2 mb-8"]
                [ h1_
                    [class_ "text-3xl font-semibold tracking-tight"]
                    ["Kitchen Sink"]
                , p_
                    [class_ "text-muted-foreground"]
                    [ "A collection of all the components available in miso-ui / Basecoat."
                    ]
                ]
            , div_
                [class_ "grid gap-4 flex-1"]
                [ section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "accordion"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Accordion"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/accordion"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "group grid w-full max-w-xl gap-4"]
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
                                                "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline "
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
                                                "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline "
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
                                                "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline "
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
                                            [class_ "text-sm"]
                                            [ "Yes. It's animated by default, but you can disable it if you prefer."
                                            ]
                                        ]
                                    ]
                                ]
                            , section_
                                [class_ "accordion"]
                                [ details_
                                    [class_ "group border-b last:border-b-0"]
                                    [ summary_
                                        [ class_
                                            "w-full focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] transition-all outline-none rounded-md"
                                        ]
                                        [ h2_
                                            [ class_
                                                "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline "
                                            ]
                                            [ "What are the key considerations when implementing a comprehensive enterprise-level authentication system?"
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
                                            [ "Implementing a robust enterprise authentication system requires careful consideration of multiple factors. This includes secure password hashing and storage, multi-factor authentication (MFA) implementation, session management, OAuth2 and SSO integration, regular security audits, rate limiting to prevent brute force attacks, and maintaining detailed audit logs. Additionally, you'll need to consider scalability, performance impact, and compliance with relevant data protection regulations such as GDPR or HIPAA."
                                            ]
                                        ]
                                    ]
                                , details_
                                    [class_ "group"]
                                    [ summary_
                                        [ class_
                                            "w-full focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] transition-all outline-none rounded-md"
                                        ]
                                        [ h2_
                                            [ class_
                                                "flex flex-1 items-start justify-between gap-4 py-4 text-left text-sm font-medium hover:underline "
                                            ]
                                            [ "How does modern distributed system architecture handle eventual consistency and data synchronization across multiple regions?"
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
                                            [class_ "text-sm"]
                                            [ "Modern distributed systems employ various strategies to maintain data consistency across regions. This often involves using techniques like CRDT (Conflict-Free Replicated Data Types), vector clocks, and gossip protocols. Systems might implement event sourcing patterns, utilize message queues for asynchronous updates, and employ sophisticated conflict resolution strategies. Popular solutions like Amazon's DynamoDB and Google's Spanner demonstrate different approaches to solving these challenges, balancing between consistency, availability, and partition tolerance as described in the CAP theorem."
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "alert"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Alert"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/alert"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "grid max-w-xl items-start gap-4"]
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
                                            "m19 21-7-4-7 4V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2Z"
                                        ]
                                    , path_ [d_ "m9 10 2 2 4-4"]
                                    ]
                                , section_
                                    []
                                    [ "This is an alert with icon, description and no title."
                                    ]
                                ]
                            , div_
                                [class_ "alert"]
                                [ section_
                                    []
                                    [ "This one has a description only. No title. No icon."
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
                                , h2_ [] ["Let's try one with icon and title."]
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
                                    [ rect_
                                        [ rx_ "1"
                                        , height_ "4"
                                        , width_ "18"
                                        , y_ "8"
                                        , x_ "3"
                                        ]
                                    , path_ [d_ "M12 8v13"]
                                    , path_
                                        [d_ "M19 12v7a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2v-7"]
                                    , path_
                                        [ d_
                                            "M7.5 8a2.5 2.5 0 0 1 0-5A4.8 8 0 0 1 12 8a4.8 8 0 0 1 4.5-5 2.5 2.5 0 0 1 0 5"
                                        ]
                                    ]
                                , section_
                                    []
                                    [ "This is a very long alert description that demonstrates how the component handles extended text content and potentially wraps across multiple lines"
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
                                    [ circle_ [r_ "10", cy_ "12", cx_ "12"]
                                    , line_
                                        [y2_ "12", y1_ "8", x2_ "12", x1_ "12"]
                                    , line_
                                        [y2_ "16", y1_ "16", x2_ "12.01", x1_ "12"]
                                    ]
                                , h2_
                                    []
                                    [ "This is an extremely long alert title that spans multiple lines to demonstrate how the component handles very lengthy headings while maintaining readability and proper text wrapping behavior"
                                    ]
                                , section_
                                    []
                                    [ "This is an equally long description that contains detailed information about the alert. It shows how the component can accommodate extensive content while preserving proper spacing, alignment, and readability across different screen sizes and viewport widths. This helps ensure the user experience remains consistent regardless of the content length."
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
                                    , line_
                                        [y2_ "12", y1_ "8", x2_ "12", x1_ "12"]
                                    , line_
                                        [y2_ "16", y1_ "16", x2_ "12.01", x1_ "12"]
                                    ]
                                , h2_ [] ["Something went wrong!"]
                                , section_
                                    []
                                    [ p_
                                        []
                                        [ "Please verify your billing information and try again."
                                        ]
                                    , ul_
                                        []
                                        [ li_ [] ["Check your card details"]
                                        , li_ [] ["Ensure sufficient funds"]
                                        , li_ [] ["Verify billing address"]
                                        ]
                                    ]
                                ]
                            , div_
                                [ class_
                                    "alert border-amber-50 bg-amber-50 text-amber-900 dark:border-amber-950 dark:bg-amber-950 dark:text-amber-100"
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
                                    , path_ [d_ "m9 12 2 2 4-4"]
                                    ]
                                , h2_
                                    []
                                    ["Plot Twist: This Alert is Actually Amber!"]
                                , section_
                                    []
                                    [ "This one has custom colors for light and dark mode."
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "alert-dialog"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_
                            [class_ "text-sm font-medium"]
                            ["Alert Dialog"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/alert-dialog"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ button_
                            [class_ "btn-outline", type_ "button"]
                            ["Open dialog"]
                        , dialog_
                            [ aria_
                                "describedby"
                                "alert-dialog-demo-description"
                            , aria_ "labelledby" "alert-dialog-demo-title"
                            , class_ "dialog "
                            , id_ "alert-dialog-demo"
                            ]
                            [ div_
                                []
                                [ header_
                                    []
                                    [ h2_
                                        [id_ "alert-dialog-demo-title"]
                                        ["Are you absolutely sure?"]
                                    , p_
                                        [id_ "alert-dialog-demo-description"]
                                        [ "This action cannot be undone. This will permanently delete your account and remove your data from our servers."
                                        ]
                                    ]
                                , footer_
                                    []
                                    [ button_ [class_ "btn-outline"] ["Cancel"]
                                    , button_ [class_ "btn-primary"] ["Continue"]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "avatar"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Avatar"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/avatar"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [ class_
                                "flex flex-row flex-wrap items-center gap-4"
                            ]
                            [ img_
                                [ src_ "https://github.com/dmjio.png"
                                , alt_ "@dmjio"
                                , class_ "size-8 shrink-0 object-cover rounded-full"
                                ]
                            , span_
                                [ class_
                                    "size-8 shrink-0 bg-muted flex items-center justify-center rounded-full"
                                ]
                                ["CN"]
                            , img_
                                [ src_ "https://github.com/dmjio.png"
                                , alt_ "@dmjio"
                                , class_
                                    "size-12 shrink-0 object-cover rounded-full"
                                ]
                            , img_
                                [ src_ "https://github.com/dmjio.png"
                                , alt_ "@dmjio"
                                , class_ "size-8 shrink-0 object-cover rounded-lg"
                                ]
                            , div_
                                [ class_
                                    "flex -space-x-2 [&_img]:ring-background [&_img]:ring-2 [&_img]:grayscale [&_img]:size-8 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full" 
                                ]
                                [ img_
                                    [ src_ "https://github.com/dmjio.png"
                                    , alt_ "@dmjio"
                                    ]
                                , img_
                                    [ src_ "https://github.com/shadcn.png"
                                    , alt_ "@shadcn"
                                    ]
                                , img_
                                    [ src_ "https://github.com/adamwathan.png"
                                    , alt_ "@adamwathan"
                                    ]
                                , img_
                                    [ src_ "https://github.com/hunvreus.png"
                                    , alt_ "@hunvreus"
                                    ]
                                ]
                            , div_
                                [ class_
                                    "flex -space-x-2 [&_img]:ring-background [&_img]:ring-2 [&_img]:grayscale [&_img]:size-12 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full"
                                ]
                                [ img_
                                    [ src_ "https://github.com/dmjio.png"
                                    , alt_ "@dmjio"
                                    ]
                                , img_
                                    [ src_ "https://github.com/shadcn.png"
                                    , alt_ "@shadcn"
                                    ]
                                , img_
                                    [ src_ "https://github.com/adamwathan.png"
                                    , alt_ "@adamwathan"
                                    ]
                                , img_
                                    [ src_ "https://github.com/hunvreus.png"
                                    , alt_ "@hunvreus"
                                    ]
                                ]
                            , div_
                                [ class_
                                    "flex -space-x-2 hover:space-x-1 [&_img]:ring-background [&_img]:size-12 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full [&_img]:ring-2 [&_img]:grayscale [&_img]:transition-all [&_img]:ease-in-out [&_img]:duration-300"
                                ]
                                [ img_
                                    [ src_ "https://github.com/dmjio.png"
                                    , alt_ "@dmjio"
                                    ]
                                , img_
                                    [ src_ "https://github.com/shadcn.png"
                                    , alt_ "@shadcn"
                                    ]
                                , img_
                                    [ src_ "https://github.com/adamwathan.png"
                                    , alt_ "@adamwathan"
                                    ]
                                , img_
                                    [ src_ "https://github.com/hunvreus.png"
                                    , alt_ "@hunvreus"
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "badge"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Badge"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/badge"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-2"]
                            [ div_
                                [ class_
                                    "flex flex-wrap items-center gap-2 md:flex-row"
                                ]
                                [ span_ [class_ "badge"] ["Primary"]
                                , span_ [class_ "badge-secondary"] ["Secondary"]
                                , span_ [class_ "badge-outline"] ["Outline"]
                                , span_
                                    [class_ "badge-destructive"]
                                    ["Destructive"]
                                , span_
                                    [class_ "badge-outline"]
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
                                        [path_ [d_ "M20 6 9 17l-5-5"]]
                                    , "Badge"
                                    ]
                                , span_
                                    [class_ "badge-destructive"]
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
                                    , "Alert"
                                    ]
                                , span_
                                    [class_ "badge rounded-full min-w-5 px-1"]
                                    ["8"]
                                , span_
                                    [ class_
                                        "badge-destructive rounded-full min-w-5 px-1"
                                    ]
                                    ["99"]
                                , span_
                                    [ class_
                                        "badge-outline rounded-full min-w-5 px-1 font-mono tabular-nums"
                                    ]
                                    ["20+"]
                                ]
                            , div_
                                [ class_
                                    "flex flex-wrap items-center gap-2 md:flex-row"
                                ]
                                [ a_
                                    [class_ "badge", href_ "#"]
                                    [ "Link"
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
                                    [class_ "badge-secondary", href_ "#"]
                                    [ "Link"
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
                                    [class_ "badge-destructive", href_ "#"]
                                    [ "Link"
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
                                    [class_ "badge-outline", href_ "#"]
                                    [ "Link"
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
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "breadcrumb"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Breadcrumb"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/breadcrumb"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ ol_
                            [ class_
                                "text-muted-foreground flex flex-wrap items-center gap-1.5 text-sm break-words sm:gap-2.5"
                            ]
                            [ li_
                                [class_ "inline-flex items-center gap-1.5"]
                                [ a_
                                    [ class_ "hover:text-foreground transition-colors"
                                    , href_ "#"
                                    ]
                                    ["Home"]
                                ]
                            , li_
                                []
                                [ svg_
                                    [ class_ "size-3.5"
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
                                    [path_ [d_ "m9 18 6-6-6-6"]]
                                ]
                            , li_
                                [class_ "inline-flex items-center gap-1.5"]
                                [ div_
                                    [ data_ "dropdown-menu-initialized" "true"
                                    , class_ "dropdown-menu "
                                    , id_ "demo-breadcrumb-menu"
                                    ]
                                    [ button_
                                        [ class_
                                            "flex size-9 items-center justify-center h-4 w-4 hover:text-foreground cursor-pointer"
                                        , aria_ "expanded" "false"
                                        , aria_ "controls" "demo-breadcrumb-menu-menu"
                                        , aria_ "haspopup" "menu"
                                        , id_ "demo-breadcrumb-menu-trigger"
                                        , type_ "button"
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
                                            [ circle_ [r_ "1", cy_ "12", cx_ "12"]
                                            , circle_ [r_ "1", cy_ "12", cx_ "19"]
                                            , circle_ [r_ "1", cy_ "12", cx_ "5"]
                                            ]
                                        ]
                                    , div_
                                        [ class_ "p-1"
                                        , aria_ "hidden" "true"
                                        , data_ "popover" ""
                                        , id_ "demo-breadcrumb-menu-popover"
                                        ]
                                        [ div_
                                            [ aria_ "labelledby" "demo-breadcrumb-menu-trigger"
                                            , id_ "demo-breadcrumb-menu-menu"
                                            , role_ "menu"
                                            ]
                                            [ div_
                                                [ role_ "menuitem"
                                                , id_ "demo-breadcrumb-menu-items-1"
                                                ]
                                                ["Documentation"]
                                            , div_
                                                [ role_ "menuitem"
                                                , id_ "demo-breadcrumb-menu-items-2"
                                                ]
                                                ["Themes"]
                                            , div_
                                                [ role_ "menuitem"
                                                , id_ "demo-breadcrumb-menu-items-3"
                                                ]
                                                ["GitHub"]
                                            ]
                                        ]
                                    ]
                                ]
                            , li_
                                []
                                [ svg_
                                    [ class_ "size-3.5"
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
                                    [path_ [d_ "m9 18 6-6-6-6"]]
                                ]
                            , li_
                                [class_ "inline-flex items-center gap-1.5"]
                                [ a_
                                    [ class_ "hover:text-foreground transition-colors"
                                    , href_ "#"
                                    ]
                                    ["Components"]
                                ]
                            , li_
                                []
                                [ svg_
                                    [ class_ "size-3.5"
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
                                    [path_ [d_ "m9 18 6-6-6-6"]]
                                ]
                            , li_
                                [class_ "inline-flex items-center gap-1.5"]
                                [ span_
                                    [class_ "text-foreground font-normal"]
                                    ["Breadcrumb"]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "button"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Button"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/button"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-6"]
                            [ div_
                                [ class_
                                    "flex flex-wrap items-center gap-2 md:flex-row"
                                ]
                                [ button_ [class_ "btn-primary"] ["Primary"]
                                , button_ [class_ "btn-outline"] ["Outline"]
                                , button_ [class_ "btn-ghost"] ["Ghost"]
                                , button_
                                    [class_ "btn-destructive"]
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
                                                "M14.536 21.686a.5.5 0 0 0 .937-.024l6.5-19a.496.496 0 0 0-.635-.635l-19 6.5a.5.5 0 0 0-.024.937l7.93 3.18a2 2 0 0 1 1.112 1.11z"
                                            ]
                                        , path_ [d_ "m21.854 2.147-10.94 10.939"]
                                        ]
                                    , "Danger"
                                    ]
                                , button_ [class_ "btn-secondary"] ["Secondary"]
                                , button_ [class_ "btn-link"] ["Link"]
                                , button_
                                    [class_ "btn-outline"]
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
                                                "M14.536 21.686a.5.5 0 0 0 .937-.024l6.5-19a.496.496 0 0 0-.635-.635l-19 6.5a.5.5 0 0 0-.024.937l7.93 3.18a2 2 0 0 1 1.112 1.11z"
                                            ]
                                        , path_ [d_ "m21.854 2.147-10.94 10.939"]
                                        ]
                                    , "Send"
                                    ]
                                , button_
                                    [class_ "btn-outline"]
                                    [ "Learn more"
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
                                , button_
                                    [textProp "disabled" "", class_ "btn-outline"]
                                    [ svg_
                                        [ class_ "animate-spin"
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
                                        [ path_ [d_ "M12 2v4"]
                                        , path_ [d_ "m16.2 7.8 2.9-2.9"]
                                        , path_ [d_ "M18 12h4"]
                                        , path_ [d_ "m16.2 16.2 2.9 2.9"]
                                        , path_ [d_ "M12 18v4"]
                                        , path_ [d_ "m4.9 19.1 2.9-2.9"]
                                        , path_ [d_ "M2 12h4"]
                                        , path_ [d_ "m4.9 4.9 2.9 2.9"]
                                        ]
                                    , "Loading"
                                    ]
                                ]
                            , div_
                                [ class_
                                    "flex flex-wrap items-center gap-2 md:flex-row"
                                ]
                                [ button_ [class_ "btn-sm-primary"] ["Primary"]
                                , button_ [class_ "btn-sm-outline"] ["Outline"]
                                , button_ [class_ "btn-sm-ghost"] ["Ghost"]
                                , button_ [class_ "btn-sm-destructive"] ["Danger"]
                                , button_ [class_ "btn-sm-secondary"] ["Secondary"]
                                , button_ [class_ "btn-sm-link"] ["Link"]
                                , button_
                                    [class_ "btn-sm-outline"]
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
                                                "M14.536 21.686a.5.5 0 0 0 .937-.024l6.5-19a.496.496 0 0 0-.635-.635l-19 6.5a.5.5 0 0 0-.024.937l7.93 3.18a2 2 0 0 1 1.112 1.11z"
                                            ]
                                        , path_ [d_ "m21.854 2.147-10.94 10.939"]
                                        ]
                                    , "Send"
                                    ]
                                , button_
                                    [class_ "btn-sm-outline"]
                                    [ "Learn more"
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
                                , button_
                                    [textProp "disabled" "", class_ "btn-sm-outline"]
                                    [ svg_
                                        [ class_ "animate-spin"
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
                                        [ path_ [d_ "M12 2v4"]
                                        , path_ [d_ "m16.2 7.8 2.9-2.9"]
                                        , path_ [d_ "M18 12h4"]
                                        , path_ [d_ "m16.2 16.2 2.9 2.9"]
                                        , path_ [d_ "M12 18v4"]
                                        , path_ [d_ "m4.9 19.1 2.9-2.9"]
                                        , path_ [d_ "M2 12h4"]
                                        , path_ [d_ "m4.9 4.9 2.9 2.9"]
                                        ]
                                    , "Loading"
                                    ]
                                ]
                            , div_
                                [ class_
                                    "flex flex-wrap items-center gap-2 md:flex-row"
                                ]
                                [ button_ [class_ "btn-lg-primary"] ["Primary"]
                                , button_ [class_ "btn-lg-outline"] ["Outline"]
                                , button_ [class_ "btn-lg-ghost"] ["Ghost"]
                                , button_
                                    [class_ "btn-lg-destructive"]
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
                                                "M14.536 21.686a.5.5 0 0 0 .937-.024l6.5-19a.496.496 0 0 0-.635-.635l-19 6.5a.5.5 0 0 0-.024.937l7.93 3.18a2 2 0 0 1 1.112 1.11z"
                                            ]
                                        , path_ [d_ "m21.854 2.147-10.94 10.939"]
                                        ]
                                    , "Danger"
                                    ]
                                , button_ [class_ "btn-lg-secondary"] ["Secondary"]
                                , button_ [class_ "btn-lg-link"] ["Link"]
                                , button_
                                    [class_ "btn-lg-outline"]
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
                                                "M14.536 21.686a.5.5 0 0 0 .937-.024l6.5-19a.496.496 0 0 0-.635-.635l-19 6.5a.5.5 0 0 0-.024.937l7.93 3.18a2 2 0 0 1 1.112 1.11z"
                                            ]
                                        , path_ [d_ "m21.854 2.147-10.94 10.939"]
                                        ]
                                    , "Send"
                                    ]
                                , button_
                                    [class_ "btn-lg-outline"]
                                    [ "Learn more"
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
                                , button_
                                    [textProp "disabled" "", class_ "btn-lg-outline"]
                                    [ svg_
                                        [ class_ "animate-spin"
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
                                        [ path_ [d_ "M12 2v4"]
                                        , path_ [d_ "m16.2 7.8 2.9-2.9"]
                                        , path_ [d_ "M18 12h4"]
                                        , path_ [d_ "m16.2 16.2 2.9 2.9"]
                                        , path_ [d_ "M12 18v4"]
                                        , path_ [d_ "m4.9 19.1 2.9-2.9"]
                                        , path_ [d_ "M2 12h4"]
                                        , path_ [d_ "m4.9 4.9 2.9 2.9"]
                                        ]
                                    , "Loading"
                                    ]
                                ]
                            , div_
                                [ class_
                                    "flex flex-wrap items-center gap-2 md:flex-row"
                                ]
                                [ button_
                                    [class_ "btn-icon-primary"]
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
                                            [d_ "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"]
                                        , polyline_ [points_ "7 10 12 15 17 10"]
                                        , line_
                                            [y2_ "3", y1_ "15", x2_ "12", x1_ "12"]

                                        ]
                                    ]
                                , button_
                                    [class_ "btn-icon-secondary"]
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
                                            [d_ "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"]
                                        , polyline_ [points_ "17 8 12 3 7 8"]
                                        , line_
                                            [y2_ "15", y1_ "3", x2_ "12", x1_ "12"]
                                        ]
                                    ]
                                , button_
                                    [class_ "btn-icon-outline"]
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
                                        [ path_ [d_ "M5 12h14"]
                                        , path_ [d_ "m12 5 7 7-7 7"]
                                        ]
                                    ]
                                , button_
                                    [class_ "btn-icon-ghost"]
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
                                        [ circle_ [r_ "1", cy_ "12", cx_ "12"]
                                        , circle_ [r_ "1", cy_ "12", cx_ "19"]
                                        , circle_ [r_ "1", cy_ "12", cx_ "5"]
                                        ]
                                    ]
                                , button_
                                    [class_ "btn-icon-destructive"]
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
                                        [ path_ [d_ "M3 6h18"]
                                        , path_
                                            [d_ "M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6"]
                                        , path_
                                            [d_ "M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2"]
                                        , line_
                                            [y2_ "17", y1_ "11", x2_ "10", x1_ "10"]
                                        , line_
                                            [y2_ "17", y1_ "11", x2_ "14", x1_ "14"]
                                        ]
                                    ]
                                , button_
                                    [ textProp "disabled" ""
                                    , class_ "btn-icon-outline"
                                    ]
                                    [ svg_
                                        [ class_ "animate-spin"
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
                                        [ path_ [d_ "M12 2v4"]
                                        , path_ [d_ "m16.2 7.8 2.9-2.9"]
                                        , path_ [d_ "M18 12h4"]
                                        , path_ [d_ "m16.2 16.2 2.9 2.9"]
                                        , path_ [d_ "M12 18v4"]
                                        , path_ [d_ "m4.9 19.1 2.9-2.9"]
                                        , path_ [d_ "M2 12h4"]
                                        , path_ [d_ "m4.9 4.9 2.9 2.9"]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "card"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Card"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/card"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col items-start gap-4"]
                            [ div_
                                [class_ "card w-full max-w-sm"]
                                [ header_
                                    []
                                    [ h2_ [] ["Login to your account"]
                                    , p_
                                        []
                                        [ "Enter your details below to login to your account"
                                        ]
                                    ]
                                , section_
                                    []
                                    [ form_
                                        [class_ "form grid gap-6"]
                                        [ div_
                                            [class_ "grid gap-2"]
                                            [ label_ [for_ "demo-card-form-email"] ["Email"]
                                            , input_
                                                [id_ "demo-card-form-email", type_ "email"]
                                            ]
                                        , div_
                                            [class_ "grid gap-2"]
                                            [ div_
                                                [class_ "flex items-center gap-2"]
                                                [ label_
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
                                        ]
                                    ]
                                , footer_
                                    [class_ "flex flex-col items-center gap-2"]
                                    [ button_
                                        [class_ "btn w-full", type_ "button"]
                                        ["Login"]
                                    , button_
                                        [class_ "btn-outline w-full", type_ "button"]
                                        ["Login with Google"]
                                    , p_
                                        [class_ "mt-4 text-center text-sm"]
                                        [ "Don't have an account?"
                                        , a_
                                            [ class_ "underline-offset-4 hover:underline"
                                            , href_ "#"
                                            ]
                                            ["Sign up"]
                                        ]
                                    ]
                                ]
                            , div_
                                [class_ "card"]
                                [ header_
                                    []
                                    [ h2_ [] ["Meeting Notes"]
                                    , p_
                                        []
                                        ["Transcript from the meeting with the client."]
                                    ]
                                , section_
                                    [class_ "text-sm"]
                                    [ p_
                                        []
                                        [ "Client requested dashboard redesign with focus on mobile responsiveness."
                                        ]
                                    , ol_
                                        [ class_
                                            "mt-4 flex list-decimal flex-col gap-2 pl-6"
                                        ]
                                        [ li_
                                            []
                                            ["New analytics widgets for daily/weekly metrics"]
                                        , li_ [] ["Simplified navigation menu"]
                                        , li_ [] ["Dark mode support"]
                                        , li_ [] ["Timeline: 6 weeks"]
                                        , li_
                                            []
                                            ["Follow-up meeting scheduled for next Tuesday"]
                                        ]
                                    ]
                                , footer_
                                    [class_ "flex items-center"]
                                    [ div_
                                        [ class_
                                            "flex -space-x-2 [&_img]:ring-background [&_img]:ring-2 [&_img]:grayscale [&_img]:size-8 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full"
                                        ]
                                        [ img_
                                            [ src_ "https://github.com/dmjio.png"
                                            , alt_ "@dmjio"
                                            ]
                                        , img_
                                            [ src_ "https://github.com/shadcn.png"
                                            , alt_ "@shadcn"
                                            ]
                                        , img_
                                            [ src_ "https://github.com/adamwathan.png"
                                            , alt_ "@adamwathan"
                                            ]
                                        ]
                                    ]
                                ]
                            , div_
                                [class_ "card"]
                                [ header_
                                    []
                                    [ h2_ [] ["Is this an image?"]
                                    , p_ [] ["This is a card with an image."]
                                    ]
                                , section_
                                    [class_ "px-0"]
                                    [ img_
                                        [ CSS.style_ ["color" =: "transparent"]
                                        , class_ "aspect-video object-cover"
                                        , height_ "500"
                                        , width_ "500"
                                        , loading_ "lazy"
                                        , alt_ "Photo by Drew Beamer"
                                        , src_ "https://images.unsplash.com/photo-1588345921523-c2dcdb7f1dcd?w=800&dpr=2&q=80&w=1080&q=75"
                                        ]
                                    ]
                                , footer_
                                    [class_ "flex items-center gap-2"]
                                    [ span_
                                        [class_ "badge-outline"]
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
                                            [ path_ [d_ "M2 4v16"]
                                            , path_ [d_ "M2 8h18a2 2 0 0 1 2 2v10"]
                                            , path_ [d_ "M2 17h20"]
                                            , path_ [d_ "M6 8v9"]
                                            ]
                                        , "1"
                                        ]
                                    , span_
                                        [class_ "badge-outline"]
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
                                            [ path_ [d_ "M10 4 8 6"]
                                            , path_ [d_ "M17 19v2"]
                                            , path_ [d_ "M2 12h20"]
                                            , path_ [d_ "M7 19v2"]
                                            , path_
                                                [ d_
                                                    "M9 5 7.621 3.621A2.121 2.121 0 0 0 4 5v12a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-5"
                                                ]
                                            ]
                                        , "2"
                                        ]
                                    , span_
                                        [class_ "badge-outline"]
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
                                            [ path_ [d_ "m12 8 6-3-6-3v10"]
                                            , path_
                                                [ d_
                                                    "m8 11.99-5.5 3.14a1 1 0 0 0 0 1.74l8.5 4.86a2 2 0 0 0 2 0l8.5-4.86a1 1 0 0 0 0-1.74L16 12"
                                                ]
                                            , path_ [d_ "m6.49 12.85 11.02 6.3"]
                                            , path_ [d_ "M17.51 12.85 6.5 19.15"]
                                            ]
                                        , "350m²"
                                        ]
                                    , span_
                                        [class_ "ml-auto font-medium tabular-nums"]
                                        ["$135,000"]
                                    ]
                                ]
                            , div_
                                [ class_
                                    "flex w-full flex-wrap items-start gap-8 md:*:[.card]:basis-1/4"
                                ]
                                [ div_
                                    [class_ "card"]
                                    [section_ [] ["Content Only"]]
                                , div_
                                    [class_ "card"]
                                    [ header_
                                        []
                                        [ h2_ [] ["Header Only"]
                                        , p_
                                            []
                                            [ "This is a card with a header and a description."
                                            ]
                                        ]
                                    ]
                                , div_
                                    [class_ "card"]
                                    [ header_
                                        []
                                        [ h2_ [] ["Header and Content"]
                                        , p_
                                            []
                                            ["This is a card with a header and a content."]
                                        ]
                                    , section_ [] ["Content only."]
                                    ]
                                , div_ [class_ "card"] [footer_ [] ["Footer Only"]]
                                , div_
                                    [class_ "card"]
                                    [ header_
                                        []
                                        [ h2_ [] ["Header + Footer"]
                                        , p_
                                            []
                                            ["This is a card with a header and a footer."]
                                        ]
                                    , footer_ [] ["Footer"]
                                    ]
                                , div_
                                    [class_ "card"]
                                    [section_ [] ["Content"], footer_ [] ["Footer"]]
                                , div_
                                    [class_ "card"]
                                    [ header_
                                        []
                                        [ h2_ [] ["Header + Content + Footer"]
                                        , p_
                                            []
                                            [ "This is a card with a header, content and footer."
                                            ]
                                        ]
                                    , section_ [] ["Content"]
                                    , footer_ [] ["Footer"]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "checkbox"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Checkbox"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/checkbox"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-6 max-w-lg"]
                            [ label_
                                [class_ "label gap-3"]
                                [ input_ [class_ "input", type_ "checkbox"]
                                , "Accept terms and conditions"
                                ]
                            , div_
                                [class_ "flex items-start gap-3"]
                                [ input_
                                    [ class_ "input"
                                    , id_ "demo-checkbox-label-and-description"
                                    , type_ "checkbox"
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ label_
                                        [ class_ "label"
                                        , for_ "demo-checkbox-label-and-description"
                                        ]
                                        ["Accept terms and conditions"]
                                    , p_
                                        [class_ "text-muted-foreground text-sm"]
                                        [ "By clicking this checkbox, you agree to the terms and conditions."
                                        ]
                                    ]
                                ]
                            , label_
                                [class_ "label gap-3"]
                                [ input_
                                    [ textProp "disabled" ""
                                    , class_ "input"
                                    , type_ "checkbox"
                                    ]
                                , "Enable notifications"
                                ]
                            , label_
                                [ class_
                                    "flex items-start gap-3 border p-3 hover:bg-accent/50 rounded-lg has-[input[type='checkbox']:checked]:border-blue-600 has-[input[type='checkbox']:checked]:bg-blue-50 dark:has-[input[type='checkbox']:checked]:border-blue-900 dark:has-[input[type='checkbox']:checked]:bg-blue-950"
                                ]
                                [ input_
                                    [ checked_ True
                                    , class_
                                        "input checked:bg-blue-600 checked:border-blue-600 dark:checked:bg-blue-700 dark:checked:border-blue-700 checked:after:bg-white"
                                    , type_ "checkbox"
                                    ]
                                , div_
                                    [class_ "grid gap-2"]
                                    [ h2_
                                        [class_ "text-sm leading-none font-medium"]
                                        ["Enable notifications"]
                                    , p_
                                        [class_ "text-muted-foreground text-sm"]
                                        [ "You can enable or disable notifications at any time."
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "combobox"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Combobox"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/combobox"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-wrap items-start gap-4"]
                            [ div_
                                [ data_ "select-initialized" "true"
                                , class_ "select "
                                , id_ "select-242298"
                                ]
                                [ button_
                                    [ aria_ "controls" "select-242298-listbox"
                                    , aria_ "expanded" "false"
                                    , aria_ "haspopup" "listbox"
                                    , id_ "select-242298-trigger"
                                    , class_ "btn-outline justify-between font-normal "
                                    , type_ "button"
                                    ]
                                    [ span_ [class_ "truncate"] ["Next.js"]
                                    , svg_
                                        [ class_
                                            "lucide lucide-chevrons-up-down-icon lucide-chevrons-up-down text-muted-foreground opacity-50 shrink-0"
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
                                        [ path_ [d_ "m7 15 5 5 5-5"]
                                        , path_ [d_ "m7 9 5-5 5 5"]
                                        ]
                                    ]
                                , div_
                                    [ class_ "w-48"
                                    , aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "select-242298-popover"
                                    ]
                                    [ header_
                                        []
                                        [ svg_
                                            [ class_ "lucide lucide-search-icon lucide-search"
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
                                            [ circle_ [r_ "8", cy_ "11", cx_ "11"]
                                            , path_ [d_ "m21 21-4.3-4.3"]
                                            ]
                                        , input_
                                            [ aria_ "labelledby" "select-242298-trigger"
                                            , aria_ "controls" "select-242298-listbox"
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
                                        [ data_ "empty" "No framework found."
                                        , aria_ "labelledby" "select-242298-trigger"
                                        , aria_ "orientation" "vertical"
                                        , id_ "select-242298-listbox"
                                        , role_ "listbox"
                                        ]
                                        [ div_
                                            [ aria_ "selected" "true"
                                            , data_ "value" "Next.js"
                                            , role_ "option"
                                            ]
                                            ["Next.js"]
                                        , div_
                                            [data_ "value" "SvelteKit", role_ "option"]
                                            ["SvelteKit"]
                                        , div_
                                            [data_ "value" "Nuxt.js", role_ "option"]
                                            ["Nuxt.js"]
                                        , div_
                                            [data_ "value" "Remix", role_ "option"]
                                            ["Remix"]
                                        , div_
                                            [data_ "value" "Astro", role_ "option"]
                                            ["Astro"]
                                        ]
                                    ]
                                , input_
                                    [ value_ "Next.js"
                                    , name_ "select-242298-value"
                                    , type_ "hidden"
                                    ]
                                ]
                            , div_
                                [ data_ "protonpass-form" ""
                                , data_ "select-initialized" "true"
                                , class_ "select "
                                , id_ "select-342556"
                                ]
                                [ button_
                                    [ aria_ "controls" "select-342556-listbox"
                                    , aria_ "expanded" "false"
                                    , aria_ "haspopup" "listbox"
                                    , id_ "select-342556-trigger"
                                    , class_ "btn-outline justify-between font-normal "
                                    , type_ "button"
                                    ]
                                    [ span_ [class_ "truncate"] ["(GMT-5) New York"]
                                    , svg_
                                        [ class_
                                            "lucide lucide-chevrons-up-down-icon lucide-chevrons-up-down text-muted-foreground opacity-50 shrink-0"
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
                                        [ path_ [d_ "m7 15 5 5 5-5"]
                                        , path_ [d_ "m7 9 5-5 5 5"]
                                        ]
                                    ]
                                , div_
                                    [ class_ "w-72"
                                    , aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "select-342556-popover"
                                    ]
                                    [ header_
                                        []
                                        [ svg_
                                            [ class_ "lucide lucide-search-icon lucide-search"
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
                                            [ circle_ [r_ "8", cy_ "11", cx_ "11"]
                                            , path_ [d_ "m21 21-4.3-4.3"]
                                            ]
                                        , input_
                                            [ aria_ "labelledby" "select-342556-trigger"
                                            , aria_ "controls" "select-342556-listbox"
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
                                        [ data_ "empty" "No timezone found."
                                        , aria_ "labelledby" "select-342556-trigger"
                                        , aria_ "orientation" "vertical"
                                        , id_ "select-342556-listbox"
                                        , role_ "listbox"
                                        ]
                                        [ div_
                                            [class_ "max-h-64 overflow-y-auto scrollbar"]
                                            [ div_
                                                [ aria_
                                                    "labelledby"
                                                    "demo-combobox-timezones-group-0"
                                                , role_ "group"
                                                ]
                                                [ span_
                                                    [ role_ "heading"
                                                    , id_ "demo-combobox-timezones-group-0"
                                                    ]
                                                    ["Americas"]
                                                , div_
                                                    [ aria_ "selected" "true"
                                                    , data_ "value" "America/New_York"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT-5) New York"]
                                                , div_
                                                    [ data_ "value" "America/Los_Angeles"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT-8) Los Angeles"]
                                                , div_
                                                    [ data_ "value" "America/Chicago"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT-6) Chicago"]
                                                , div_
                                                    [ data_ "value" "America/Toronto"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT-5) Toronto"]
                                                , div_
                                                    [ data_ "value" "America/Vancouver"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT-8) Vancouver"]
                                                , div_
                                                    [ data_ "value" "America/Sao_Paulo"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT-3) São Paulo"]
                                                ]
                                            , div_
                                                [ aria_
                                                    "labelledby"
                                                    "demo-combobox-timezones-group-1"
                                                , role_ "group"
                                                ]
                                                [ span_
                                                    [ role_ "heading"
                                                    , id_ "demo-combobox-timezones-group-1"
                                                    ]
                                                    ["Europe"]
                                                , div_
                                                    [data_ "value" "Europe/London", role_ "option"]
                                                    ["(GMT+0) London"]
                                                , div_
                                                    [data_ "value" "Europe/Paris", role_ "option"]
                                                    ["(GMT+1) Paris"]
                                                , div_
                                                    [data_ "value" "Europe/Berlin", role_ "option"]
                                                    ["(GMT+1) Berlin"]
                                                , div_
                                                    [data_ "value" "Europe/Rome", role_ "option"]
                                                    ["(GMT+1) Rome"]
                                                , div_
                                                    [data_ "value" "Europe/Madrid", role_ "option"]
                                                    ["(GMT+1) Madrid"]
                                                , div_
                                                    [ data_ "value" "Europe/Amsterdam"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT+1) Amsterdam"]
                                                ]
                                            , div_
                                                [ aria_
                                                    "labelledby"
                                                    "demo-combobox-timezones-group-2"
                                                , role_ "group"
                                                ]
                                                [ span_
                                                    [ role_ "heading"
                                                    , id_ "demo-combobox-timezones-group-2"
                                                    ]
                                                    ["Asia/Pacific"]
                                                , div_
                                                    [data_ "value" "Asia/Tokyo", role_ "option"]
                                                    ["(GMT+9) Tokyo"]
                                                , div_
                                                    [data_ "value" "Asia/Shanghai", role_ "option"]
                                                    ["(GMT+8) Shanghai"]
                                                , div_
                                                    [ data_ "value" "Asia/Singapore"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT+8) Singapore"]
                                                , div_
                                                    [data_ "value" "Asia/Dubai", role_ "option"]
                                                    ["(GMT+4) Dubai"]
                                                , div_
                                                    [ data_ "value" "Australia/Sydney"
                                                    , role_ "option"
                                                    ]
                                                    ["(GMT+11) Sydney"]
                                                , div_
                                                    [data_ "value" "Asia/Seoul", role_ "option"]
                                                    ["(GMT+9) Seoul"]
                                                ]
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "option"]
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
                                                , path_ [d_ "M8 12h8"]
                                                , path_ [d_ "M12 8v8"]
                                                ]
                                            , "Create timezone"
                                            ]
                                        ]
                                    ]
                                , input_
                                    [ value_ "America/New_York"
                                    , name_ "select-342556-value"
                                    , type_ "hidden"
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "dialog"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Dialog"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/dialog"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [ data_ "protonpass-form" ""
                            , class_ "flex flex-wrap items-center gap-4"
                            ]
                            [ button_
                                [class_ "btn-outline", type_ "button"]
                                ["Edit Profile"]
                            , dialog_
                                [ aria_
                                    "describedby"
                                    "demo-dialog-edit-profile-description"
                                , aria_
                                    "labelledby"
                                    "demo-dialog-edit-profile-title"
                                , class_
                                    "dialog w-full sm:max-w-[425px] max-h-[612px]"
                                , id_ "demo-dialog-edit-profile"
                                ]
                                [ div_
                                    []
                                    [ header_
                                        []
                                        [ h2_
                                            [id_ "demo-dialog-edit-profile-title"]
                                            ["Edit profile"]
                                        , p_
                                            [id_ "demo-dialog-edit-profile-description"]
                                            [ "Make changes to your profile here. Click save when you're done."
                                            ]
                                        ]
                                    , section_
                                        []
                                        [ form_
                                            [class_ "form grid gap-4"]
                                            [ div_
                                                [class_ "grid gap-3"]
                                                [ label_
                                                    [for_ "demo-dialog-edit-profile-name"]
                                                    ["Name"]
                                                , input_
                                                    [ id_ "demo-dialog-edit-profile-name"
                                                    , value_ "Pedro Duarte"
                                                    , type_ "text"
                                                    ]
                                                ]
                                            , div_
                                                [class_ "grid gap-3"]
                                                [ label_
                                                    [for_ "demo-dialog-edit-profile-username"]
                                                    ["Username"]
                                                , input_
                                                    [ id_ "demo-dialog-edit-profile-username"
                                                    , value_ "@peduarte"
                                                    , type_ "text"
                                                    ]
                                                ]
                                            ]
                                        ]
                                    , footer_
                                        []
                                        [ button_ [class_ "btn-outline"] ["Cancel"]
                                        , button_ [class_ "btn"] ["Save changes"]
                                        ]
                                    , button_
                                        [aria_ "label" "Close dialog", type_ "button"]
                                        [ svg_
                                            [ class_ "lucide lucide-x-icon lucide-x"
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
                                            [ path_ [d_ "M18 6 6 18"]
                                            , path_ [d_ "m6 6 12 12"]
                                            ]
                                        ]
                                    ]
                                ]
                            , button_
                                [class_ "btn-outline", type_ "button"]
                                ["Scrollable Content"]
                            , dialog_
                                [ aria_ "describedby" "dialog-example-description"
                                , aria_ "labelledby" "dialog-example-title"
                                , class_
                                    "dialog w-full sm:max-w-[425px] max-h-[612px]"
                                , id_ "dialog-example"
                                ]
                                [ div_
                                    []
                                    [ header_
                                        []
                                        [ h2_
                                            [id_ "dialog-example-title"]
                                            ["Scrollable Content"]
                                        , p_
                                            [id_ "dialog-example-description"]
                                            ["This is a dialog with scrollable content."]
                                        ]
                                    , section_
                                        [class_ "overflow-y-auto scrollbar"]
                                        [ div_
                                            [class_ "space-y-4 text-sm"]
                                            [ p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            , p_
                                                []
                                                [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                                ]
                                            ]
                                        ]
                                    , footer_
                                        []
                                        [button_ [class_ "btn-outline"] ["Close"]]
                                    , button_
                                        [aria_ "label" "Close dialog", type_ "button"]
                                        [ svg_
                                            [ class_ "lucide lucide-x-icon lucide-x"
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
                                            [ path_ [d_ "M18 6 6 18"]
                                            , path_ [d_ "m6 6 12 12"]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "dropdown-menu"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_
                            [class_ "text-sm font-medium"]
                            ["Dropdown Menu"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/dropdown-menu"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-wrap items-start gap-4"]
                            [ div_
                                [ data_ "dropdown-menu-initialized" "true"
                                , class_ "dropdown-menu "
                                , id_ "dropdown-menu-default"
                                ]
                                [ button_
                                    [ class_ "btn-outline"
                                    , aria_ "expanded" "false"
                                    , aria_ "controls" "dropdown-menu-default-menu"
                                    , aria_ "haspopup" "menu"
                                    , id_ "dropdown-menu-default-trigger"
                                    , type_ "button"
                                    ]
                                    ["Open"]
                                , div_
                                    [ class_ "min-w-56"
                                    , aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "dropdown-menu-default-popover"
                                    ]
                                    [ div_
                                        [ aria_ "labelledby" "dropdown-menu-default-trigger"
                                        , id_ "dropdown-menu-default-menu"
                                        , role_ "menu"
                                        ]
                                        [ div_
                                            [ aria_ "labelledby" "account-options"
                                            , role_ "group"
                                            ]
                                            [ div_
                                                [id_ "account-options", role_ "heading"]
                                                ["My Account"]
                                            , div_
                                                [role_ "menuitem"]
                                                [ "Profile"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⇧⌘P"]
                                                ]
                                            , div_
                                                [role_ "menuitem"]
                                                [ "Billing"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘B"]
                                                ]
                                            , div_
                                                [role_ "menuitem"]
                                                [ "Settings"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘S"]
                                                ]
                                            , div_
                                                [role_ "menuitem"]
                                                [ "Keyboard shortcuts"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘K"]
                                                ]
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_ [role_ "menuitem"] ["GitHub"]
                                        , div_ [role_ "menuitem"] ["Support"]
                                        , div_
                                            [aria_ "disabled" "true", role_ "menuitem"]
                                            ["API"]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
                                            [ "Logout"
                                            , span_
                                                [ class_
                                                    "text-muted-foreground ml-auto text-xs tracking-widest"
                                                ]
                                                ["⇧⌘P"]
                                            ]
                                        ]
                                    ]
                                ]
                            , div_
                                [ data_ "dropdown-menu-initialized" "true"
                                , class_ "dropdown-menu "
                                , id_ "dropdown-menu-checkboxes"
                                ]
                                [ button_
                                    [ class_ "btn-outline"
                                    , aria_ "expanded" "false"
                                    , aria_ "controls" "dropdown-menu-checkboxes-menu"
                                    , aria_ "haspopup" "menu"
                                    , id_ "dropdown-menu-checkboxes-trigger"
                                    , type_ "button"
                                    ]
                                    ["Checkboxes"]
                                , div_
                                    [ class_ "min-w-56"
                                    , aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "dropdown-menu-checkboxes-popover"
                                    ]
                                    [ div_
                                        [ aria_
                                            "labelledby"
                                            "dropdown-menu-checkboxes-trigger"
                                        , id_ "dropdown-menu-checkboxes-menu"
                                        , role_ "menu"
                                        ]
                                        [ div_
                                            [ aria_ "labelledby" "account-options"
                                            , role_ "group"
                                            ]
                                            [ div_
                                                [id_ "account-options", role_ "heading"]
                                                ["Account Options"]
                                            , div_
                                                [role_ "menuitem"]
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
                                                        [d_ "M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2"]
                                                    , circle_ [r_ "4", cy_ "7", cx_ "12"]
                                                    ]
                                                , "Profile"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⇧⌘P"]
                                                ]
                                            , div_
                                                [role_ "menuitem"]
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
                                                        [ rx_ "2"
                                                        , y_ "5"
                                                        , x_ "2"
                                                        , height_ "14"
                                                        , width_ "20"
                                                        ]
                                                    , line_
                                                        [y2_ "10", y1_ "10", x2_ "22", x1_ "2"]
                                                    ]
                                                , "Billing"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘B"]
                                                ]
                                            , div_
                                                [role_ "menuitem"]
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
                                                            "M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"
                                                        ]
                                                    , circle_ [r_ "3", cy_ "12", cx_ "12"]
                                                    ]
                                                , "Settings"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘S"]
                                                ]
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [ aria_ "labelledby" "appearance-options"
                                            , role_ "group"
                                            ]
                                            [ div_
                                                [id_ "appearance-options", role_ "heading"]
                                                ["Appearance"]
                                            , div_
                                                [ class_ "group"
                                                , aria_ "checked" "true"
                                                , role_ "menuitemcheckbox"
                                                ]
                                                [ svg_
                                                    [ focusable_ "false"
                                                    , aria_ "hidden" "true"
                                                    , class_ "invisible group-aria-checked:visible"
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
                                                , "Status Bar"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⇧⌘P"]
                                                ]
                                            , div_
                                                [ aria_ "disabled" "true"
                                                , class_ "group"
                                                , aria_ "checked" "false"
                                                , role_ "menuitemcheckbox"
                                                ]
                                                [ svg_
                                                    [ focusable_ "false"
                                                    , aria_ "hidden" "true"
                                                    , class_ "invisible group-aria-checked:visible"
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
                                                , "Activity Bar"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘B"]
                                                ]
                                            , div_
                                                [ class_ "group"
                                                , aria_ "checked" "false"
                                                , role_ "menuitemcheckbox"
                                                ]
                                                [ svg_
                                                    [ focusable_ "false"
                                                    , aria_ "hidden" "true"
                                                    , class_ "invisible group-aria-checked:visible"
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
                                                , "Panel"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘S"]
                                                ]
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [d_ "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"]
                                                , polyline_ [points_ "16 17 21 12 16 7"]
                                                , line_
                                                    [y2_ "12", y1_ "12", x2_ "9", x1_ "21"]
                                                ]
                                            , "Logout"
                                            , span_
                                                [ class_
                                                    "text-muted-foreground ml-auto text-xs tracking-widest"
                                                ]
                                                ["⇧⌘P"]
                                            ]
                                        ]
                                    ]
                                ]
                            , div_
                                [ data_ "dropdown-menu-initialized" "true"
                                , class_ "dropdown-menu "
                                , id_ "dropdown-menu-radio-group"
                                ]
                                [ button_
                                    [ class_ "btn-outline"
                                    , aria_ "expanded" "false"
                                    , aria_ "controls" "dropdown-menu-radio-group-menu"
                                    , aria_ "haspopup" "menu"
                                    , id_ "dropdown-menu-radio-group-trigger"
                                    , type_ "button"
                                    ]
                                    ["Radio Group"]
                                , div_
                                    [ class_ "min-w-56"
                                    , aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "dropdown-menu-radio-group-popover"
                                    ]
                                    [ div_
                                        [ aria_
                                            "labelledby"
                                            "dropdown-menu-radio-group-trigger"
                                        , id_ "dropdown-menu-radio-group-menu"
                                        , role_ "menu"
                                        ]
                                        [ div_
                                            [ aria_ "labelledby" "position-options"
                                            , role_ "group"
                                            ]
                                            [ span_
                                                [role_ "heading", id_ "position-options"]
                                                ["Panel Position"]
                                            , hr_ [role_ "separator"]
                                            , div_
                                                [ class_ "group"
                                                , aria_ "checked" "false"
                                                , role_ "menuitemradio"
                                                ]
                                                [ div_
                                                    [ class_ "size-4 flex items-center justify-center"
                                                    ]
                                                    [ div_
                                                        [ focusable_ "false"
                                                        
                                                        , aria_ "hidden" "true"
                                                        , class_
                                                            "size-2 rounded-full bg-foreground invisible group-aria-checked:visible"
                                                        ]
                                                        []
                                                    ]
                                                , "Status Bar"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⇧⌘P"]
                                                ]
                                            , div_
                                                [ class_ "group"
                                                , aria_ "checked" "true"
                                                , role_ "menuitemradio"
                                                ]
                                                [ div_
                                                    [ class_ "size-4 flex items-center justify-center"
                                                    ]
                                                    [ div_
                                                        [ focusable_ "false"
                                                        
                                                        , aria_ "hidden" "true"
                                                        , class_
                                                            "size-2 rounded-full bg-foreground invisible group-aria-checked:visible"
                                                        ]
                                                        []
                                                    ]
                                                , "Activity Bar"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘B"]
                                                ]
                                            , div_
                                                [ class_ "group"
                                                , aria_ "checked" "false"
                                                , role_ "menuitemradio"
                                                ]
                                                [ div_
                                                    [ class_ "size-4 flex items-center justify-center"
                                                    ]
                                                    [ div_
                                                        [ focusable_ "false"
                                                        
                                                        , aria_ "hidden" "true"
                                                        , class_
                                                            "size-2 rounded-full bg-foreground invisible group-aria-checked:visible"
                                                        ]
                                                        []
                                                    ]
                                                , "Panel"
                                                , span_
                                                    [ class_
                                                        "text-muted-foreground ml-auto text-xs tracking-widest"
                                                    ]
                                                    ["⌘S"]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            , div_
                                [ data_ "dropdown-menu-initialized" "true"
                                , class_ "dropdown-menu "
                                , id_ "dropdown-checkboxes"
                                ]
                                [ button_
                                    [ class_
                                        "btn-outline h-12 justify-start px-2 md:max-w-[200px]"
                                    , aria_ "expanded" "false"
                                    , aria_ "controls" "dropdown-checkboxes-menu"
                                    , aria_ "haspopup" "menu"
                                    , id_ "dropdown-checkboxes-trigger"
                                    , type_ "button"
                                    ]
                                    [ img_
                                        [ class_ "size-8 shrink-0 rounded-full"
                                        , src_ "https://github.com/dmjio.png"
                                        , alt_ "@dmjio"
                                        ]
                                    , div_
                                        [ class_
                                            "grid flex-1 text-left text-sm leading-tight"
                                        ]
                                        [ span_
                                            [class_ "truncate font-medium"]
                                            ["dmjio"]
                                        , span_
                                            [ class_ "text-muted-foreground truncate text-xs"
                                            ]
                                            ["dmjio@example.com"]
                                        ]
                                    , svg_
                                        [ class_ "text-muted-foreground ml-auto"
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
                                        [ path_ [d_ "m7 15 5 5 5-5"]
                                        , path_ [d_ "m7 9 5-5 5 5"]
                                        ]
                                    ]
                                , div_
                                    [ aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "dropdown-checkboxes-popover"
                                    ]
                                    [ div_
                                        [ aria_ "labelledby" "dropdown-checkboxes-trigger"
                                        , id_ "dropdown-checkboxes-menu"
                                        , role_ "menu"
                                        ]
                                        [ div_
                                            [ class_
                                                "flex items-center gap-2 px-1 py-1.5 text-left text-sm"
                                            ]
                                            [ img_
                                                [ class_ "size-8 shrink-0 rounded-full"
                                                , src_ "https://github.com/dmjio.png"
                                                , alt_ "@dmjio"
                                                ]
                                            , div_
                                                [ class_
                                                    "grid flex-1 text-left text-sm leading-tight"
                                                ]
                                                [ span_
                                                    [class_ "truncate font-medium"]
                                                    ["dmjio"]
                                                , span_
                                                    [ class_ "text-muted-foreground truncate text-xs"
                                                    ]
                                                    ["dmjio@example.com"]
                                                ]
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
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
                                                        "M11.525 2.295a.53.53 0 0 1 .95 0l2.31 4.679a2.123 2.123 0 0 0 1.595 1.16l5.166.756a.53.53 0 0 1 .294.904l-3.736 3.638a2.123 2.123 0 0 0-.611 1.878l.882 5.14a.53.53 0 0 1-.771.56l-4.618-2.428a2.122 2.122 0 0 0-1.973 0L6.396 21.01a.53.53 0 0 1-.77-.56l.881-5.139a2.122 2.122 0 0 0-.611-1.879L2.16 9.795a.53.53 0 0 1 .294-.906l5.165-.755a2.122 2.122 0 0 0 1.597-1.16z"
                                                    ]
                                                ]
                                            , "Upgrade to Pro"
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [d_ "M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2"]
                                                , circle_ [r_ "4", cy_ "7", cx_ "12"]
                                                ]
                                            , "Account"
                                            ]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [ rx_ "2"
                                                    , y_ "5"
                                                    , x_ "2"
                                                    , height_ "14"
                                                    , width_ "20"
                                                    ]
                                                , line_
                                                    [y2_ "10", y1_ "10", x2_ "22", x1_ "2"]
                                                ]
                                            , "Billing"
                                            ]
                                        , div_
                                            [role_ "menuitem"]
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
                                                [ path_ [d_ "M10.268 21a2 2 0 0 0 3.464 0"]
                                                , path_
                                                    [ d_
                                                        "M3.262 15.326A1 1 0 0 0 4 17h16a1 1 0 0 0 .74-1.673C19.41 13.956 18 12.499 18 8A6 6 0 0 0 6 8c0 4.499-1.411 5.956-2.738 7.326"
                                                    ]
                                                ]
                                            , "Notifications"
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [d_ "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"]
                                                , polyline_ [points_ "16 17 21 12 16 7"]
                                                , line_
                                                    [y2_ "12", y1_ "12", x2_ "9", x1_ "21"]
                                                ]
                                            , "Signout"
                                            ]
                                        ]
                                    ]
                                ]
                            , div_
                                [ data_ "dropdown-menu-initialized" "true"
                                , class_ "dropdown-menu "
                                , id_ "dropdown-checkboxes"
                                ]
                                [ button_
                                    [ class_ "btn-icon-ghost rounded-full size-8"
                                    , aria_ "expanded" "false"
                                    , aria_ "controls" "dropdown-checkboxes-menu"
                                    , aria_ "haspopup" "menu"
                                    , id_ "dropdown-checkboxes-trigger"
                                    , type_ "button"
                                    ]
                                    [ img_
                                        [ class_ "size-8 shrink-0 rounded-full"
                                        , src_ "https://github.com/dmjio.png"
                                        , alt_ "@dmjio"
                                        ]
                                    ]
                                , div_
                                    [ data_ "align" "end"
                                    , aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "dropdown-checkboxes-popover"
                                    ]
                                    [ div_
                                        [ aria_ "labelledby" "dropdown-checkboxes-trigger"
                                        , id_ "dropdown-checkboxes-menu"
                                        , role_ "menu"
                                        ]
                                        [ div_
                                            [ class_
                                                "flex items-center gap-2 px-1 py-1.5 text-left text-sm"
                                            ]
                                            [ img_
                                                [ class_ "size-8 shrink-0 rounded-full"
                                                , src_ "https://github.com/dmjio.png"
                                                , alt_ "@dmjio"
                                                ]
                                            , div_
                                                [ class_
                                                    "grid flex-1 text-left text-sm leading-tight"
                                                ]
                                                [ span_
                                                    [class_ "truncate font-medium"]
                                                    ["dmjio"]
                                                , span_
                                                    [ class_ "text-muted-foreground truncate text-xs"
                                                    ]
                                                    ["dmjio@example.com"]
                                                ]
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
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
                                                        "M11.525 2.295a.53.53 0 0 1 .95 0l2.31 4.679a2.123 2.123 0 0 0 1.595 1.16l5.166.756a.53.53 0 0 1 .294.904l-3.736 3.638a2.123 2.123 0 0 0-.611 1.878l.882 5.14a.53.53 0 0 1-.771.56l-4.618-2.428a2.122 2.122 0 0 0-1.973 0L6.396 21.01a.53.53 0 0 1-.77-.56l.881-5.139a2.122 2.122 0 0 0-.611-1.879L2.16 9.795a.53.53 0 0 1 .294-.906l5.165-.755a2.122 2.122 0 0 0 1.597-1.16z"
                                                    ]
                                                ]
                                            , "Upgrade to Pro"
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [d_ "M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2"]
                                                , circle_ [r_ "4", cy_ "7", cx_ "12"]
                                                ]
                                            , "Account"
                                            ]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [ rx_ "2"
                                                    , y_ "5"
                                                    , x_ "2"
                                                    , height_ "14"
                                                    , width_ "20"
                                                    ]
                                                , line_
                                                    [y2_ "10", y1_ "10", x2_ "22", x1_ "2"]
                                                ]
                                            , "Billing"
                                            ]
                                        , div_
                                            [role_ "menuitem"]
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
                                                [ path_ [d_ "M10.268 21a2 2 0 0 0 3.464 0"]
                                                , path_
                                                    [ d_
                                                        "M3.262 15.326A1 1 0 0 0 4 17h16a1 1 0 0 0 .74-1.673C19.41 13.956 18 12.499 18 8A6 6 0 0 0 6 8c0 4.499-1.411 5.956-2.738 7.326"
                                                    ]
                                                ]
                                            , "Notifications"
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [d_ "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"]
                                                , polyline_ [points_ "16 17 21 12 16 7"]
                                                , line_
                                                    [y2_ "12", y1_ "12", x2_ "9", x1_ "21"]
                                                ]
                                            , "Signout"
                                            ]
                                        ]
                                    ]
                                ]
                            , div_
                                [ data_ "dropdown-menu-initialized" "true"
                                , class_ "dropdown-menu "
                                , id_ "dropdown-checkboxes"
                                ]
                                [ button_
                                    [ class_ "btn-icon-ghost"
                                    , aria_ "expanded" "false"
                                    , aria_ "controls" "dropdown-checkboxes-menu"
                                    , aria_ "haspopup" "menu"
                                    , id_ "dropdown-checkboxes-trigger"
                                    , type_ "button"
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
                                        [ circle_ [r_ "1", cy_ "12", cx_ "12"]
                                        , circle_ [r_ "1", cy_ "12", cx_ "19"]
                                        , circle_ [r_ "1", cy_ "12", cx_ "5"]
                                        ]
                                    ]
                                , div_
                                    [ class_ "min-w-32"
                                    , aria_ "hidden" "true"
                                    , data_ "popover" ""
                                    , id_ "dropdown-checkboxes-popover"
                                    ]
                                    [ div_
                                        [ aria_ "labelledby" "dropdown-checkboxes-trigger"
                                        , id_ "dropdown-checkboxes-menu"
                                        , role_ "menu"
                                        ]
                                        [ div_
                                            [role_ "menuitem"]
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
                                                        "M21.174 6.812a1 1 0 0 0-3.986-3.987L3.842 16.174a2 2 0 0 0-.5.83l-1.321 4.352a.5.5 0 0 0 .623.622l4.353-1.32a2 2 0 0 0 .83-.497z"
                                                    ]
                                                , path_ [d_ "m15 5 4 4"]
                                                ]
                                            , "Edit"
                                            ]
                                        , div_
                                            [role_ "menuitem"]
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
                                                    [d_ "M4 12v8a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-8"]
                                                , polyline_ [points_ "16 6 12 2 8 6"]
                                                , line_
                                                    [y2_ "15", y1_ "2", x2_ "12", x1_ "12"]
                                                ]
                                            , "Share"
                                            ]
                                        , hr_ [role_ "separator"]
                                        , div_
                                            [ class_
                                                "text-destructive hover:bg-destructive/10 dark:hover:bg-destructive/20 focus:bg-destructive/10 dark:focus:bg-destructive/20 focus:text-destructive [&_svg]:!text-destructive"
                                            , role_ "menuitem"
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
                                                [ path_ [d_ "M3 6h18"]
                                                , path_
                                                    [d_ "M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6"]
                                                , path_
                                                    [d_ "M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2"]
                                                , line_
                                                    [y2_ "17", y1_ "11", x2_ "10", x1_ "10"]
                                                , line_
                                                    [y2_ "17", y1_ "11", x2_ "14", x1_ "14"]
                                                ]
                                            , "Delete"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-16"
                    , id_ "form"
                    ]
                    [ header_
                        [class_ "border-b px-4 py-3"]
                        [h2_ [class_ "text-sm font-medium"] ["Form"]]
                    , div_
                        [class_ "p-4"]
                        [ form_
                            [class_ "form grid w-full max-w-sm gap-6"]
                            [ div_
                                [class_ "grid gap-2"]
                                [ label_ [for_ "demo-form-text"] ["Username"]
                                , input_
                                    [ placeholder_ "dmjio"
                                    , id_ "demo-form-text"
                                    , type_ "text"
                                    ]
                                , p_
                                    [class_ "text-muted-foreground text-sm"]
                                    ["This is your public display name."]
                                ]
                            , div_
                                [class_ "grid gap-2"]
                                [ label_ [for_ "demo-form-select"] ["Email"]
                                , select_
                                    [id_ "demo-form-select"]
                                    [ option_
                                        [value_ "bob@example.com"]
                                        ["m@example.com"]
                                    , option_
                                        [value_ "alice@example.com"]
                                        ["m@google.com"]
                                    , option_
                                        [value_ "john@example.com"]
                                        ["m@support.com"]
                                    ]
                                , p_
                                    [class_ "text-muted-foreground text-sm"]
                                    [ "You can manage email addresses in your email settings."
                                    ]
                                ]
                            , div_
                                [class_ "grid gap-2"]
                                [ label_ [for_ "demo-form-text"] ["Bio"]
                                , textarea_
                                    [ rows_ "3"
                                    , placeholder_ "I like to..."
                                    , id_ "demo-form-textarea"
                                    ]
                                    []
                                , p_
                                    [class_ "text-muted-foreground text-sm"]
                                    [ "You can @mention other users and organizations."
                                    ]
                                ]
                            , div_
                                [class_ "flex flex-col gap-3"]
                                [ label_
                                    [for_ "demo-form-radio"]
                                    ["Notify me about..."]
                                , fieldset_
                                    [class_ "grid gap-3", id_ "demo-form-radio"]
                                    [ label_
                                        [class_ "font-normal"]
                                        [ input_
                                            [ value_ "1"
                                            , name_ "demo-form-radio"
                                            , type_ "radio"
                                            ]
                                        , "All new messages"
                                        ]
                                    , label_
                                        [class_ "font-normal"]
                                        [ input_
                                            [ value_ "2"
                                            , name_ "demo-form-radio"
                                            , type_ "radio"
                                            ]
                                        , "Direct messages and mentions"
                                        ]
                                    , label_
                                        [class_ "font-normal"]
                                        [ input_
                                            [ value_ "3"
                                            , name_ "demo-form-radio"
                                            , type_ "radio"
                                            ]
                                        , "Nothing"
                                        ]
                                    ]
                                ]
                            , div_
                                [ class_
                                    "flex flex-row items-start gap-3 rounded-md border p-4 shadow-xs"
                                ]
                                [ input_
                                    [id_ "demo-form-checkbox", type_ "checkbox"]
                                , div_
                                    [class_ "flex flex-col gap-1"]
                                    [ label_
                                        [ class_ "leading-snug"
                                        , for_ "demo-form-checkbox"
                                        ]
                                        ["Use different settings for my mobile devices"]
                                    , p_
                                        [ class_
                                            "text-muted-foreground text-sm leading-snug"
                                        ]
                                        [ "You can manage your mobile notifications in the mobile settings page."
                                        ]
                                    ]
                                ]
                            , div_
                                [class_ "flex flex-col gap-4"]
                                [ header_
                                    []
                                    [ label_
                                        [ class_ "text-base leading-normal"
                                        , for_ "demo-form-checkboxes"
                                        ]
                                        ["Sidebar"]
                                    , p_
                                        [class_ "text-muted-foreground text-sm"]
                                        [ "Select the items you want to display in the sidebar."
                                        ]
                                    ]
                                , fieldset_
                                    [ class_ "flex flex-col gap-2"
                                    , id_ "demo-form-checkboxes"
                                    ]
                                    [ label_
                                        [class_ "font-normal leading-tight"]
                                        [ input_
                                            [ checked_ True
                                            , value_ "1"
                                            , name_ "demo-form-checkboxes"
                                            , type_ "checkbox"
                                            ]
                                        , "Recents"
                                        ]
                                    , label_
                                        [class_ "font-normal leading-tight"]
                                        [ input_
                                            [ checked_ True
                                            , value_ "2"
                                            , name_ "demo-form-checkboxes"
                                            , type_ "checkbox"
                                            ]
                                        , "Home"
                                        ]
                                    , label_
                                        [class_ "font-normal leading-tight"]
                                        [ input_
                                            [ value_ "3"
                                            , name_ "demo-form-checkboxes"
                                            , type_ "checkbox"
                                            ]
                                        , "Applications"
                                        ]
                                    , label_
                                        [class_ "font-normal leading-tight"]
                                        [ input_
                                            [ value_ "4"
                                            , name_ "demo-form-checkboxes"
                                            , type_ "checkbox"
                                            ]
                                        , "Desktop"
                                        ]
                                    , label_
                                        [class_ "font-normal leading-tight"]
                                        [ input_
                                            [ value_ "5"
                                            , name_ "demo-form-checkboxes"
                                            , type_ "checkbox"
                                            ]
                                        , "Download"
                                        ]
                                    , label_
                                        [class_ "font-normal leading-tight"]
                                        [ input_
                                            [ value_ "6"
                                            , name_ "demo-form-checkboxes"
                                            , type_ "checkbox"
                                            ]
                                        , "Documents"
                                        ]
                                    ]
                                ]
                            , div_
                                [class_ "grid gap-2"]
                                [ label_ [for_ "demo-form-date"] ["Date of birth"]
                                , input_ [id_ "demo-form-date", type_ "date"]
                                , p_
                                    [class_ "text-muted-foreground text-sm"]
                                    [ "Your date of birth is used to calculate your age."
                                    ]
                                ]
                            , section_
                                [class_ "grid gap-4"]
                                [ h3_
                                    [class_ "text-lg font-medium"]
                                    ["Email Notifications"]
                                , div_
                                    [ class_
                                        "gap-2 flex flex-row items-start justify-between rounded-lg border p-4 shadow-xs"
                                    ]
                                    [ div_
                                        [class_ "flex flex-col gap-0.5"]
                                        [ label_
                                            [ class_ "leading-normal"
                                            , for_ "demo-form-switch"
                                            ]
                                            ["Marketing emails"]
                                        , p_
                                            [class_ "text-muted-foreground text-sm"]
                                            [ "Receive emails about new products, features, and more."
                                            ]
                                        ]
                                    , input_
                                        [ role_ "switch"
                                        , id_ "demo-form-switch"
                                        , type_ "checkbox"
                                        ]
                                    ]
                                , div_
                                    [ class_
                                        "gap-2 flex flex-row items-start justify-between rounded-lg border p-4 shadow-xs"
                                    ]
                                    [ div_
                                        [class_ "flex flex-col gap-0.5 opacity-60"]
                                        [ label_
                                            [ class_ "leading-normal"
                                            , for_ "demo-form-switch-disabled"
                                            ]
                                            ["Marketing emails"]
                                        , p_
                                            [class_ "text-muted-foreground text-sm"]
                                            [ "Receive emails about new products, features, and more."
                                            ]
                                        ]
                                    , input_
                                        [ textProp "disabled" ""
                                        , role_ "switch"
                                        , id_ "demo-form-switch-disabled"
                                        , type_ "checkbox"
                                        ]
                                    ]
                                ]
                            , button_
                                [class_ "btn", type_ "submit"]
                                ["Submit"]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "input"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Input"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/input"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [ data_ "protonpass-form" ""
                            , class_ "flex flex-col gap-y-4"
                            ]
                            [ input_
                                [ placeholder_ "Text"
                                , class_ "input"
                                , type_ "text"
                                ]
                            , input_
                                [ placeholder_ "Disabled"
                                , textProp "disabled" ""
                                , class_ "input"
                                , type_ "text"
                                ]
                            , input_
                                [ placeholder_ "Error"
                                , aria_ "invalid" "true"
                                , class_ "input"
                                , type_ "text"
                                ]
                            , input_
                                [ placeholder_ "Email"
                                , class_ "input"
                                , type_ "email"
                                ]
                            , input_
                                [ placeholder_ "Password"
                                , class_ "input"
                                , type_ "password"
                                ]
                            , input_
                                [ placeholder_ "Number"
                                , class_ "input"
                                , type_ "number"
                                ]
                            , input_ [class_ "input", type_ "file"]
                            , input_
                                [ placeholder_ "Tel"
                                , class_ "input"
                                , type_ "tel"
                                ]
                            , input_
                                [ placeholder_ "URL"
                                , class_ "input"
                                , type_ "url"
                                ]
                            , input_
                                [ placeholder_ "Search"
                                , class_ "input"
                                , type_ "search"
                                ]
                            , input_ [class_ "input", type_ "date"]
                            , input_ [class_ "input", type_ "datetime-local"]
                            , input_ [class_ "input", type_ "month"]
                            , input_ [class_ "input", type_ "week"]
                            , input_ [class_ "input", type_ "time"]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "label"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Label"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/label"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-y-4"]
                            [ div_
                                [ data_ "protonpass-form" ""
                                , class_ "grid w-full max-w-sm gap-6"
                                ]
                                [ div_
                                    [class_ "flex items-center gap-3"]
                                    [ input_
                                        [ class_ "input"
                                        , id_ "label-demo-terms"
                                        , type_ "checkbox"
                                        ]
                                    , label_
                                        [class_ "label", for_ "label-demo-terms"]
                                        ["Accept terms and conditions"]
                                    ]
                                , div_
                                    [class_ "grid gap-3"]
                                    [ label_
                                        [class_ "label", for_ "label-demo-text"]
                                        ["Username"]
                                    , input_
                                        [ placeholder_ "Username"
                                        , class_ "input"
                                        , id_ "label-demo-text"
                                        , type_ "text"
                                        ]
                                    ]
                                , div_
                                    [class_ "grid gap-3"]
                                    [ label_
                                        [class_ "label", for_ "label-demo-disabled"]
                                        ["Disabled"]
                                    , input_
                                        [ textProp "disabled" ""
                                        , placeholder_ "Disabled"
                                        , class_ "peer input"
                                        , id_ "label-demo-disabled"
                                        , type_ "text"
                                        ]
                                    ]
                                , div_
                                    [class_ "grid gap-3"]
                                    [ label_
                                        [class_ "label", for_ "label-demo-textarea"]
                                        ["Message"]
                                    , textarea_
                                        [ placeholder_ "Message"
                                        , class_ "textarea"
                                        , id_ "label-demo-textarea"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "pagination"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Pagination"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/pagination"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "inline-flex"]
                            [ nav_
                                [ class_ "mx-auto flex w-full justify-center"
                                , aria_ "label" "pagination"
                                , role_ "navigation"
                                ]
                                [ ul_
                                    [class_ "flex flex-row items-center gap-1"]
                                    [ li_
                                        []
                                        [ a_
                                            [class_ "btn-ghost", href_ "#"]
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
                                                [path_ [d_ "m15 18-6-6 6-6"]]
                                            , "Previous"
                                            ]
                                        ]
                                    , li_
                                        []
                                        [ a_ [class_ "btn-ghost size-9", href_ "#"] ["1"]
                                        ]
                                    , li_
                                        []
                                        [ a_
                                            [class_ "btn-outline size-9", href_ "#"]
                                            ["2"]
                                        ]
                                    , li_
                                        []
                                        [ a_ [class_ "btn-ghost size-9", href_ "#"] ["3"]
                                        ]
                                    , li_
                                        []
                                        [ a_
                                            [class_ "btn-icon-ghost", href_ "#"]
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
                                                [ circle_ [r_ "1", cy_ "12", cx_ "12"]
                                                , circle_ [r_ "1", cy_ "12", cx_ "19"]
                                                , circle_ [r_ "1", cy_ "12", cx_ "5"]
                                                ]
                                            ]
                                        ]
                                    , li_
                                        []
                                        [ a_
                                            [class_ "btn-ghost", href_ "#"]
                                            [ "Next"
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
                                                [path_ [d_ "m9 18 6-6-6-6"]]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "popover"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Popover"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/popover"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [ data_ "protonpass-form" ""
                            , data_ "popover-initialized" "true"
                            , class_ "popover "
                            , id_ "demo-popover"
                            ]
                            [ button_
                                [ class_ "btn-outline"
                                , aria_ "controls" "demo-popover-popover"
                                , aria_ "expanded" "false"
                                , type_ "button"
                                , id_ "demo-popover-trigger"
                                ]
                                ["Open popover"]
                            , div_
                                [ class_ "w-80"
                                , aria_ "hidden" "true"
                                , data_ "popover" ""
                                , id_ "demo-popover-popover"
                                ]
                                [ div_
                                    [class_ "grid gap-4"]
                                    [ header_
                                        [class_ "grid gap-1.5"]
                                        [ h4_
                                            [class_ "leading-none font-medium"]
                                            ["Dimensions"]
                                        , p_
                                            [class_ "text-muted-foreground text-sm"]
                                            ["Set the dimensions for the layer."]
                                        ]
                                    , form_
                                        [class_ "form grid gap-2"]
                                        [ div_
                                            [class_ "grid grid-cols-3 items-center gap-4"]
                                            [ label_ [for_ "demo-popover-width"] ["Width"]
                                            , input_
                                                [ class_ "col-span-2 h-8"
                                                , value_ "100%"
                                                , id_ "demo-popover-width"
                                                , type_ "text"
                                                ]
                                            ]
                                        , div_
                                            [class_ "grid grid-cols-3 items-center gap-4"]
                                            [ label_
                                                [for_ "demo-popover-max-width"]
                                                ["Max. width"]
                                            , input_
                                                [ class_ "col-span-2 h-8"
                                                , value_ "300px"
                                                , id_ "demo-popover-max-width"
                                                , type_ "text"
                                                ]
                                            ]
                                        , div_
                                            [class_ "grid grid-cols-3 items-center gap-4"]
                                            [ label_ [for_ "demo-popover-height"] ["Height"]
                                            , input_
                                                [ class_ "col-span-2 h-8"
                                                , value_ "25px"
                                                , id_ "demo-popover-height"
                                                , type_ "text"
                                                ]
                                            ]
                                        , div_
                                            [class_ "grid grid-cols-3 items-center gap-4"]
                                            [ label_
                                                [for_ "demo-popover-max-height"]
                                                ["Max. height"]
                                            , input_
                                                [ class_ "col-span-2 h-8"
                                                , value_ "none"
                                                , id_ "demo-popover-max-height"
                                                , type_ "text"
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "radio-group"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_
                            [class_ "text-sm font-medium"]
                            ["Radio Group"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/radio-group"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-y-6"]
                            [ fieldset_
                                [class_ "grid gap-3"]
                                [ label_
                                    [class_ "label"]
                                    [ input_
                                        [ class_ "input"
                                        , value_ "default"
                                        , name_ "demo-radio-group"
                                        , type_ "radio"
                                        ]
                                    , "Default"
                                    ]
                                , label_
                                    [class_ "label"]
                                    [ input_
                                        [ class_ "input"
                                        , value_ "comfortable"
                                        , name_ "demo-radio-group"
                                        , type_ "radio"
                                        ]
                                    , "Comfortable"
                                    ]
                                , label_
                                    [class_ "label"]
                                    [ input_
                                        [ class_ "input"
                                        , value_ "compact"
                                        , name_ "demo-radio-group"
                                        , type_ "radio"
                                        ]
                                    , "Compact"
                                    ]
                                ]
                            , fieldset_
                                [class_ "grid gap-3 max-w-sm"]
                                [ label_
                                    [ class_
                                        "label gap-3 items-start hover:bg-accent/50 rounded-lg border p-4 has-[input[type='radio']:checked]:border-green-600 has-[input[type='radio']:checked]:bg-green-50 dark:has-[input[type='radio']:checked]:border-green-900 dark:has-[input[type='radio']:checked]:bg-green-950"
                                    ]
                                    [ input_
                                        [ class_
                                            "input checked:bg-green-600 checked:border-green-600 dark:checked:bg-input/30 checked:before:bg-background dark:checked:before:bg-primary"
                                        , value_ "no"
                                        , name_ "demo-radio-group-styled"
                                        , type_ "radio"
                                        ]
                                    , div_
                                        [class_ "grid gap-1 font-normal"]
                                        [ h2_ [class_ "font-medium"] ["Starter Plan"]
                                        , p_
                                            [class_ "text-muted-foreground leading-snug"]
                                            [ "Perfect for small businesses getting started with our platform"
                                            ]
                                        ]
                                    ]
                                , label_
                                    [ class_
                                        "label gap-3 items-start hover:bg-accent/50 rounded-lg border p-4 has-[input[type='radio']:checked]:border-green-600 has-[input[type='radio']:checked]:bg-green-50 dark:has-[input[type='radio']:checked]:border-green-900 dark:has-[input[type='radio']:checked]:bg-green-950"
                                    ]
                                    [ input_
                                        [ class_
                                            "input checked:bg-green-600 checked:border-green-600 dark:checked:bg-input/30 checked:before:bg-background dark:checked:before:bg-primary"
                                        , value_ "no"
                                        , name_ "demo-radio-group-styled"
                                        , type_ "radio"
                                        ]
                                    , div_
                                        [class_ "grid gap-1 font-normal"]
                                        [ h2_ [class_ "font-medium"] ["Pro Plan"]
                                        , p_
                                            [class_ "text-muted-foreground leading-snug"]
                                            [ "Advanced features for growing businesses with higher demands"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "select"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Select"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/select"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4 gap"]
                        [ div_
                            [class_ "flex flex-col gap-4"]
                            [ div_
                                [ class_
                                    "flex flex-wrap items-center gap-2 md:flex-row"
                                ]
                                [ select_
                                    [class_ "select w-[180px]"]
                                    [ optgroup_
                                        [ P.label_ "Fruits"]
                                        [ option_ [] ["Apple"]
                                        , option_ [] ["Banana"]
                                        , option_ [] ["Blueberry"]
                                        ]
                                    , optgroup_
                                        [P.label_ "Grapes"]
                                        [option_ [] ["Pineapple"]]
                                    ]
                                , select_
                                    [ textProp "disabled" ""
                                    , class_ "select w-[180px]"
                                    ]
                                    [option_ [] ["Disabled"]]
                                ]
                            , div_
                                [class_ "flex flex-wrap items-center gap-4"]
                                [ div_
                                    [ class_
                                        "flex flex-wrap items-center gap-2 md:flex-row"
                                    ]
                                    [ div_
                                        [ data_ "select-initialized" "true"
                                        , class_ "select "
                                        , id_ "select-default"
                                        ]
                                        [ button_
                                            [ aria_ "controls" "select-default-listbox"
                                            , aria_ "expanded" "false"
                                            , aria_ "haspopup" "listbox"
                                            , id_ "select-default-trigger"
                                            , class_
                                                "btn-outline justify-between font-normal w-[180px]"
                                            , type_ "button"
                                            ]
                                            [ span_ [class_ "truncate"] ["Blueberry"]
                                            , svg_
                                                [ class_
                                                    "lucide lucide-chevron-down-icon lucide-chevron-down text-muted-foreground opacity-50 shrink-0"
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
                                        , div_
                                            [ aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "select-default-popover"
                                            ]
                                            [ div_
                                                [ aria_ "labelledby" "select-default-trigger"
                                                , aria_ "orientation" "vertical"
                                                , id_ "select-default-listbox"
                                                , role_ "listbox"
                                                ]
                                                [ div_
                                                    [ aria_
                                                        "labelledby"
                                                        "group-label-select-default-items-1"
                                                    , role_ "group"
                                                    ]
                                                    [ div_
                                                        [ id_ "group-label-select-default-items-1"
                                                        , role_ "heading"
                                                        ]
                                                        ["Fruits"]
                                                    , div_
                                                        [ data_ "value" "apple"
                                                        , role_ "option"
                                                        , id_ "select-default-items-1-1"
                                                        ]
                                                        ["Apple"]
                                                    , div_
                                                        [ data_ "value" "banana"
                                                        , role_ "option"
                                                        , id_ "select-default-items-1-2"
                                                        ]
                                                        ["Banana"]
                                                    , div_
                                                        [ aria_ "selected" "true"
                                                        , data_ "value" "blueberry"
                                                        , role_ "option"
                                                        , id_ "select-default-items-1-3"
                                                        ]
                                                        ["Blueberry"]
                                                    ]
                                                , div_
                                                    [ aria_
                                                        "labelledby"
                                                        "group-label-select-default-items-2"
                                                    , role_ "group"
                                                    ]
                                                    [ div_
                                                        [ id_ "group-label-select-default-items-2"
                                                        , role_ "heading"
                                                        ]
                                                        ["Grapes"]
                                                    , div_
                                                        [ data_ "value" "pineapple"
                                                        , role_ "option"
                                                        , id_ "select-default-items-2-1"
                                                        ]
                                                        ["Pineapple"]
                                                    ]
                                                ]
                                            ]
                                        , input_
                                            [ value_ "blueberry"
                                            , name_ "select-default-value"
                                            , type_ "hidden"
                                            ]
                                        ]
                                    , div_
                                        [ data_ "select-initialized" "true"
                                        , class_ "select "
                                        , id_ "select-scrollbar"
                                        ]
                                        [ button_
                                            [ aria_ "controls" "select-scrollbar-listbox"
                                            , aria_ "expanded" "false"
                                            , aria_ "haspopup" "listbox"
                                            , id_ "select-scrollbar-trigger"
                                            , class_
                                                "btn-outline justify-between font-normal w-[180px]"
                                            , type_ "button"
                                            ]
                                            [ span_ [class_ "truncate"] ["Item 0"]
                                            , svg_
                                                [ class_
                                                    "lucide lucide-chevron-down-icon lucide-chevron-down text-muted-foreground opacity-50 shrink-0"
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
                                        , div_
                                            [ aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "select-scrollbar-popover"
                                            ]
                                            [ div_
                                                [ class_ "scrollbar overflow-y-auto max-h-64"
                                                , aria_ "labelledby" "select-scrollbar-trigger"
                                                , aria_ "orientation" "vertical"
                                                , id_ "select-scrollbar-listbox"
                                                , role_ "listbox"
                                                ]
                                                [ div_
                                                    [ aria_ "selected" "true"
                                                    , data_ "value" "item-0"
                                                    , role_ "option"
                                                    ]
                                                    ["Item 0"]
                                                , div_
                                                    [data_ "value" "item-1", role_ "option"]
                                                    ["Item 1"]
                                                , div_
                                                    [data_ "value" "item-2", role_ "option"]
                                                    ["Item 2"]
                                                , div_
                                                    [data_ "value" "item-3", role_ "option"]
                                                    ["Item 3"]
                                                , div_
                                                    [data_ "value" "item-4", role_ "option"]
                                                    ["Item 4"]
                                                , div_
                                                    [data_ "value" "item-5", role_ "option"]
                                                    ["Item 5"]
                                                , div_
                                                    [data_ "value" "item-6", role_ "option"]
                                                    ["Item 6"]
                                                , div_
                                                    [data_ "value" "item-7", role_ "option"]
                                                    ["Item 7"]
                                                , div_
                                                    [data_ "value" "item-8", role_ "option"]
                                                    ["Item 8"]
                                                , div_
                                                    [data_ "value" "item-9", role_ "option"]
                                                    ["Item 9"]
                                                , div_
                                                    [data_ "value" "item-10", role_ "option"]
                                                    ["Item 10"]
                                                , div_
                                                    [data_ "value" "item-11", role_ "option"]
                                                    ["Item 11"]
                                                , div_
                                                    [data_ "value" "item-12", role_ "option"]
                                                    ["Item 12"]
                                                , div_
                                                    [data_ "value" "item-13", role_ "option"]
                                                    ["Item 13"]
                                                , div_
                                                    [data_ "value" "item-14", role_ "option"]
                                                    ["Item 14"]
                                                , div_
                                                    [data_ "value" "item-15", role_ "option"]
                                                    ["Item 15"]
                                                , div_
                                                    [data_ "value" "item-16", role_ "option"]
                                                    ["Item 16"]
                                                , div_
                                                    [data_ "value" "item-17", role_ "option"]
                                                    ["Item 17"]
                                                , div_
                                                    [data_ "value" "item-18", role_ "option"]
                                                    ["Item 18"]
                                                , div_
                                                    [data_ "value" "item-19", role_ "option"]
                                                    ["Item 19"]
                                                , div_
                                                    [data_ "value" "item-20", role_ "option"]
                                                    ["Item 20"]
                                                , div_
                                                    [data_ "value" "item-21", role_ "option"]
                                                    ["Item 21"]
                                                , div_
                                                    [data_ "value" "item-22", role_ "option"]
                                                    ["Item 22"]
                                                , div_
                                                    [data_ "value" "item-23", role_ "option"]
                                                    ["Item 23"]
                                                , div_
                                                    [data_ "value" "item-24", role_ "option"]
                                                    ["Item 24"]
                                                , div_
                                                    [data_ "value" "item-25", role_ "option"]
                                                    ["Item 25"]
                                                , div_
                                                    [data_ "value" "item-26", role_ "option"]
                                                    ["Item 26"]
                                                , div_
                                                    [data_ "value" "item-27", role_ "option"]
                                                    ["Item 27"]
                                                , div_
                                                    [data_ "value" "item-28", role_ "option"]
                                                    ["Item 28"]
                                                , div_
                                                    [data_ "value" "item-29", role_ "option"]
                                                    ["Item 29"]
                                                , div_
                                                    [data_ "value" "item-30", role_ "option"]
                                                    ["Item 30"]
                                                , div_
                                                    [data_ "value" "item-31", role_ "option"]
                                                    ["Item 31"]
                                                , div_
                                                    [data_ "value" "item-32", role_ "option"]
                                                    ["Item 32"]
                                                , div_
                                                    [data_ "value" "item-33", role_ "option"]
                                                    ["Item 33"]
                                                , div_
                                                    [data_ "value" "item-34", role_ "option"]
                                                    ["Item 34"]
                                                , div_
                                                    [data_ "value" "item-35", role_ "option"]
                                                    ["Item 35"]
                                                , div_
                                                    [data_ "value" "item-36", role_ "option"]
                                                    ["Item 36"]
                                                , div_
                                                    [data_ "value" "item-37", role_ "option"]
                                                    ["Item 37"]
                                                , div_
                                                    [data_ "value" "item-38", role_ "option"]
                                                    ["Item 38"]
                                                , div_
                                                    [data_ "value" "item-39", role_ "option"]
                                                    ["Item 39"]
                                                , div_
                                                    [data_ "value" "item-40", role_ "option"]
                                                    ["Item 40"]
                                                , div_
                                                    [data_ "value" "item-41", role_ "option"]
                                                    ["Item 41"]
                                                , div_
                                                    [data_ "value" "item-42", role_ "option"]
                                                    ["Item 42"]
                                                , div_
                                                    [data_ "value" "item-43", role_ "option"]
                                                    ["Item 43"]
                                                , div_
                                                    [data_ "value" "item-44", role_ "option"]
                                                    ["Item 44"]
                                                , div_
                                                    [data_ "value" "item-45", role_ "option"]
                                                    ["Item 45"]
                                                , div_
                                                    [data_ "value" "item-46", role_ "option"]
                                                    ["Item 46"]
                                                , div_
                                                    [data_ "value" "item-47", role_ "option"]
                                                    ["Item 47"]
                                                , div_
                                                    [data_ "value" "item-48", role_ "option"]
                                                    ["Item 48"]
                                                , div_
                                                    [data_ "value" "item-49", role_ "option"]
                                                    ["Item 49"]
                                                , div_
                                                    [data_ "value" "item-50", role_ "option"]
                                                    ["Item 50"]
                                                , div_
                                                    [data_ "value" "item-51", role_ "option"]
                                                    ["Item 51"]
                                                , div_
                                                    [data_ "value" "item-52", role_ "option"]
                                                    ["Item 52"]
                                                , div_
                                                    [data_ "value" "item-53", role_ "option"]
                                                    ["Item 53"]
                                                , div_
                                                    [data_ "value" "item-54", role_ "option"]
                                                    ["Item 54"]
                                                , div_
                                                    [data_ "value" "item-55", role_ "option"]
                                                    ["Item 55"]
                                                , div_
                                                    [data_ "value" "item-56", role_ "option"]
                                                    ["Item 56"]
                                                , div_
                                                    [data_ "value" "item-57", role_ "option"]
                                                    ["Item 57"]
                                                , div_
                                                    [data_ "value" "item-58", role_ "option"]
                                                    ["Item 58"]
                                                , div_
                                                    [data_ "value" "item-59", role_ "option"]
                                                    ["Item 59"]
                                                , div_
                                                    [data_ "value" "item-60", role_ "option"]
                                                    ["Item 60"]
                                                , div_
                                                    [data_ "value" "item-61", role_ "option"]
                                                    ["Item 61"]
                                                , div_
                                                    [data_ "value" "item-62", role_ "option"]
                                                    ["Item 62"]
                                                , div_
                                                    [data_ "value" "item-63", role_ "option"]
                                                    ["Item 63"]
                                                , div_
                                                    [data_ "value" "item-64", role_ "option"]
                                                    ["Item 64"]
                                                , div_
                                                    [data_ "value" "item-65", role_ "option"]
                                                    ["Item 65"]
                                                , div_
                                                    [data_ "value" "item-66", role_ "option"]
                                                    ["Item 66"]
                                                , div_
                                                    [data_ "value" "item-67", role_ "option"]
                                                    ["Item 67"]
                                                , div_
                                                    [data_ "value" "item-68", role_ "option"]
                                                    ["Item 68"]
                                                , div_
                                                    [data_ "value" "item-69", role_ "option"]
                                                    ["Item 69"]
                                                , div_
                                                    [data_ "value" "item-70", role_ "option"]
                                                    ["Item 70"]
                                                , div_
                                                    [data_ "value" "item-71", role_ "option"]
                                                    ["Item 71"]
                                                , div_
                                                    [data_ "value" "item-72", role_ "option"]
                                                    ["Item 72"]
                                                , div_
                                                    [data_ "value" "item-73", role_ "option"]
                                                    ["Item 73"]
                                                , div_
                                                    [data_ "value" "item-74", role_ "option"]
                                                    ["Item 74"]
                                                , div_
                                                    [data_ "value" "item-75", role_ "option"]
                                                    ["Item 75"]
                                                , div_
                                                    [data_ "value" "item-76", role_ "option"]
                                                    ["Item 76"]
                                                , div_
                                                    [data_ "value" "item-77", role_ "option"]
                                                    ["Item 77"]
                                                , div_
                                                    [data_ "value" "item-78", role_ "option"]
                                                    ["Item 78"]
                                                , div_
                                                    [data_ "value" "item-79", role_ "option"]
                                                    ["Item 79"]
                                                , div_
                                                    [data_ "value" "item-80", role_ "option"]
                                                    ["Item 80"]
                                                , div_
                                                    [data_ "value" "item-81", role_ "option"]
                                                    ["Item 81"]
                                                , div_
                                                    [data_ "value" "item-82", role_ "option"]
                                                    ["Item 82"]
                                                , div_
                                                    [data_ "value" "item-83", role_ "option"]
                                                    ["Item 83"]
                                                , div_
                                                    [data_ "value" "item-84", role_ "option"]
                                                    ["Item 84"]
                                                , div_
                                                    [data_ "value" "item-85", role_ "option"]
                                                    ["Item 85"]
                                                , div_
                                                    [data_ "value" "item-86", role_ "option"]
                                                    ["Item 86"]
                                                , div_
                                                    [data_ "value" "item-87", role_ "option"]
                                                    ["Item 87"]
                                                , div_
                                                    [data_ "value" "item-88", role_ "option"]
                                                    ["Item 88"]
                                                , div_
                                                    [data_ "value" "item-89", role_ "option"]
                                                    ["Item 89"]
                                                , div_
                                                    [data_ "value" "item-90", role_ "option"]
                                                    ["Item 90"]
                                                , div_
                                                    [data_ "value" "item-91", role_ "option"]
                                                    ["Item 91"]
                                                , div_
                                                    [data_ "value" "item-92", role_ "option"]
                                                    ["Item 92"]
                                                , div_
                                                    [data_ "value" "item-93", role_ "option"]
                                                    ["Item 93"]
                                                , div_
                                                    [data_ "value" "item-94", role_ "option"]
                                                    ["Item 94"]
                                                , div_
                                                    [data_ "value" "item-95", role_ "option"]
                                                    ["Item 95"]
                                                , div_
                                                    [data_ "value" "item-96", role_ "option"]
                                                    ["Item 96"]
                                                , div_
                                                    [data_ "value" "item-97", role_ "option"]
                                                    ["Item 97"]
                                                , div_
                                                    [data_ "value" "item-98", role_ "option"]
                                                    ["Item 98"]
                                                ]
                                            ]
                                        , input_
                                            [ value_ "item-0"
                                            , name_ "select-scrollbar-value"
                                            , type_ "hidden"
                                            ]
                                        ]
                                    , div_
                                        [ data_ "select-initialized" "true"
                                        , class_ "select "
                                        , id_ "select-disabled"
                                        ]
                                        [ button_
                                            [ disabled_
                                            , aria_ "controls" "select-disabled-listbox"
                                            , aria_ "expanded" "false"
                                            , aria_ "haspopup" "listbox"
                                            , id_ "select-disabled-trigger"
                                            , class_
                                                "btn-outline justify-between font-normal w-[180px]"
                                            , type_ "button"
                                            ]
                                            [ span_ [class_ "truncate"] ["Disabled"]
                                            , svg_
                                                [ class_
                                                    "lucide lucide-chevron-down-icon lucide-chevron-down text-muted-foreground opacity-50 shrink-0"
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
                                        , div_
                                            [ aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "select-disabled-popover"
                                            ]
                                            [ div_
                                                [ aria_ "labelledby" "select-disabled-trigger"
                                                , aria_ "orientation" "vertical"
                                                , id_ "select-disabled-listbox"
                                                , role_ "listbox"
                                                ]
                                                [ div_
                                                    [ aria_ "selected" "true"
                                                    , data_ "value" "disabled"
                                                    , role_ "option"
                                                    ]
                                                    ["Disabled"]
                                                ]
                                            ]
                                        , input_
                                            [ value_ "disabled"
                                            , name_ "select-disabled-value"
                                            , type_ "hidden"
                                            ]
                                        ]
                                    , div_
                                        [ data_ "select-initialized" "true"
                                        , class_ "select "
                                        , id_ "select-with-icon"
                                        ]
                                        [ button_
                                            [ aria_ "controls" "select-with-icon-listbox"
                                            , aria_ "expanded" "false"
                                            , aria_ "haspopup" "listbox"
                                            , id_ "select-with-icon-trigger"
                                            , class_
                                                "btn-outline justify-between font-normal w-[180px]"
                                            , type_ "button"
                                            ]
                                            [ span_
                                                [class_ "truncate"]
                                                [ span_
                                                    [class_ "flex items-center gap-2"]
                                                    [ svg_
                                                        [ class_ "text-muted-foreground"
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
                                                        [ path_ [d_ "M3 3v16a2 2 0 0 0 2 2h16"]
                                                        , path_ [d_ "M7 16h8"]
                                                        , path_ [d_ "M7 11h12"]
                                                        , path_ [d_ "M7 6h3"]
                                                        ]
                                                    , "Bar"
                                                    ]
                                                ]
                                            , svg_
                                                [ class_
                                                    "lucide lucide-chevron-down-icon lucide-chevron-down text-muted-foreground opacity-50 shrink-0"
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
                                        , div_
                                            [ aria_ "hidden" "true"
                                            , data_ "popover" ""
                                            , id_ "select-with-icon-popover"
                                            ]
                                            [ div_
                                                [ aria_ "labelledby" "select-with-icon-trigger"
                                                , aria_ "orientation" "vertical"
                                                , id_ "select-with-icon-listbox"
                                                , role_ "listbox"
                                                ]
                                                [ div_
                                                    [ aria_ "selected" "true"
                                                    , data_ "value" "bar"
                                                    , role_ "option"
                                                    , type_ "button"
                                                    ]
                                                    [ span_
                                                        [class_ "flex items-center gap-2"]
                                                        [ svg_
                                                            [ class_ "text-muted-foreground"
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
                                                            [ path_ [d_ "M3 3v16a2 2 0 0 0 2 2h16"]
                                                            , path_ [d_ "M7 16h8"]
                                                            , path_ [d_ "M7 11h12"]
                                                            , path_ [d_ "M7 6h3"]
                                                            ]
                                                        , "Bar"
                                                        ]
                                                    ]
                                                , div_
                                                    [ data_ "value" "line"
                                                    , role_ "option"
                                                    , type_ "button"
                                                    ]
                                                    [ span_
                                                        [class_ "flex items-center gap-2"]
                                                        [ svg_
                                                            [ class_ "text-muted-foreground"
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
                                                            [ path_ [d_ "M3 3v16a2 2 0 0 0 2 2h16"]
                                                            , path_ [d_ "m19 9-5 5-4-4-3 3"]
                                                            ]
                                                        , "Line"
                                                        ]
                                                    ]
                                                , div_
                                                    [ data_ "value" "pie"
                                                    , role_ "option"
                                                    , type_ "button"
                                                    ]
                                                    [ span_
                                                        [class_ "flex items-center gap-2"]
                                                        [ svg_
                                                            [ class_ "text-muted-foreground"
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
                                                            [ path_
                                                                [ d_
                                                                    "M21 12c.552 0 1.005-.449.95-.998a10 10 0 0 0-8.953-8.951c-.55-.055-.998.398-.998.95v8a1 1 0 0 0 1 1z"
                                                                ]
                                                            , path_ [d_ "M21.21 15.89A10 10 0 1 1 8 2.83"]
                                                            ]
                                                        , "Pie"
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        , input_
                                            [ value_ "bar"
                                            , name_ "chart-type"
                                            , type_ "hidden"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "skeleton"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Skeleton"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/skeleton"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-4"]
                            [ div_
                                [class_ "flex items-center gap-4"]
                                [ div_
                                    [ class_
                                        "bg-accent animate-pulse size-10 shrink-0 rounded-full"
                                    ]
                                    []
                                , div_
                                    [class_ "grid gap-2"]
                                    [ div_
                                        [ class_
                                            "bg-accent animate-pulse rounded-md h-4 w-[150px]"
                                        ]
                                        []
                                    , div_
                                        [ class_
                                            "bg-accent animate-pulse rounded-md h-4 w-[100px]"
                                        ]
                                        []
                                    ]
                                ]
                            , div_
                                [class_ "flex max-sm:flex-col gap-4 w-full"]
                                [ div_
                                    [class_ "card w-full @md:w-auto @md:min-w-sm"]
                                    [ header_
                                        []
                                        [ div_
                                            [ class_
                                                "bg-accent animate-pulse rounded-md h-4 w-2/3"
                                            ]
                                            []
                                        , div_
                                            [ class_
                                                "bg-accent animate-pulse rounded-md h-4 w-1/2"
                                            ]
                                            []
                                        ]
                                    , section_
                                        []
                                        [ div_
                                            [ class_
                                                "bg-accent animate-pulse rounded-md aspect-square w-full"
                                            ]
                                            []
                                        ]
                                    ]
                                , div_
                                    [class_ "card w-full @md:w-auto @md:min-w-sm"]
                                    [ header_
                                        []
                                        [ div_
                                            [ class_
                                                "bg-accent animate-pulse rounded-md h-4 w-2/3"
                                            ]
                                            []
                                        , div_
                                            [ class_
                                                "bg-accent animate-pulse rounded-md h-4 w-1/2"
                                            ]
                                            []
                                        ]
                                    , section_
                                        []
                                        [ div_
                                            [ class_
                                                "bg-accent animate-pulse rounded-md aspect-square w-full"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "slider"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Slider"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/slider"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "max-w-sm"]
                            [ input_
                                [ CSS.style_
                                    ["--slider-value" =: "44.44444444444444%"]
                                , value_ "12"
                                , max_ "27"
                                , min_ "0"
                                , class_ "input w-full"
                                , type_ "range"
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "switch"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Switch"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/switch"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "inline-flex flex-col gap-y-6"]
                            [ label_
                                [class_ "label"]
                                [ input_
                                    [ class_ "input"
                                    , role_ "switch"
                                    , name_ "switch"
                                    , type_ "checkbox"
                                    ]
                                , "Airplane Mode"
                                ]
                            , label_
                                [class_ "label"]
                                [ input_
                                    [ checked_ True
                                    , class_
                                        "input checked:bg-blue-500 dark:checked:bg-blue-600"
                                    , role_ "switch"
                                    , name_ "switch"
                                    , type_ "checkbox"
                                    ]
                                , "Bluetooth"
                                ]
                            , label_
                                [ class_
                                    "label gap-6 leading-none border rounded-lg p-4 has-[input[type='checkbox']:checked]:border-blue-600"
                                ]
                                [ div_
                                    [class_ "grid gap-1"]
                                    [ h2_
                                        [class_ "font-medium text-sm"]
                                        ["Share across devices"]
                                    , p_
                                        [class_ "text-muted-foreground text-sm"]
                                        [ "Focus is shared across devices, and turns off when you leave the app."
                                        ]
                                    ]
                                , input_
                                    [ class_
                                        "input checked:bg-blue-500 dark:checked:bg-blue-600"
                                    , role_ "switch"
                                    , name_ "switch"
                                    , type_ "checkbox"
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "table"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Table"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/table"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "relative w-full overflow-x-auto"]
                            [ table_
                                [class_ "table"]
                                [ caption_ [] ["A list of your recent invoices."]
                                , thead_
                                    []
                                    [ tr_
                                        []
                                        [ th_ [] ["Invoice"]
                                        , th_ [] ["Status"]
                                        , th_ [] ["Method"]
                                        , th_ [] ["Amount"]
                                        ]
                                    ]
                                , tbody_
                                    []
                                    [ tr_
                                        []
                                        [ td_ [class_ "font-medium"] ["INV001"]
                                        , td_ [] ["Paid"]
                                        , td_ [] ["Credit Card"]
                                        , td_ [class_ "text-right"] ["$250.00"]
                                        ]
                                    , tr_
                                        []
                                        [ td_ [class_ "font-medium"] ["INV002"]
                                        , td_ [] ["Pending"]
                                        , td_ [] ["PayPal"]
                                        , td_ [class_ "text-right"] ["$150.00"]
                                        ]
                                    , tr_
                                        []
                                        [ td_ [class_ "font-medium"] ["INV003"]
                                        , td_ [] ["Unpaid"]
                                        , td_ [] ["Bank Transfer"]
                                        , td_ [class_ "text-right"] ["$350.00"]
                                        ]
                                    , tr_
                                        []
                                        [ td_ [class_ "font-medium"] ["INV004"]
                                        , td_ [] ["Paid"]
                                        , td_ [] ["Paypal"]
                                        , td_ [class_ "text-right"] ["$450.00"]
                                        ]
                                    , tr_
                                        []
                                        [ td_ [class_ "font-medium"] ["INV005"]
                                        , td_ [] ["Paid"]
                                        , td_ [] ["Credit Card"]
                                        , td_ [class_ "text-right"] ["$550.00"]
                                        ]
                                    , tr_
                                        []
                                        [ td_ [class_ "font-medium"] ["INV006"]
                                        , td_ [] ["Pending"]
                                        , td_ [] ["Bank Transfer"]
                                        , td_ [class_ "text-right"] ["$200.00"]
                                        ]
                                    , tr_
                                        []
                                        [ td_ [class_ "font-medium"] ["INV007"]
                                        , td_ [] ["Unpaid"]
                                        , td_ [] ["Credit Card"]
                                        , td_ [class_ "text-right"] ["$300.00"]
                                        ]
                                    ]
                                , tfoot_
                                    []
                                    [ tr_
                                        []
                                        [ td_ [colspan_ "3"] ["Total"]
                                        , td_ [class_ "text-right"] ["$2,500.00"]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "tabs"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Tabs"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/tabs"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-6"]
                            [ div_
                                [ data_ "protonpass-form" ""
                                , data_ "tabs-initialized" "true"
                                , id_ "demo-tabs-with-panels"
                                , class_ "tabs max-w-[300px]"
                                ]
                                [ nav_
                                    [ class_ "w-full"
                                    , aria_ "orientation" "horizontal"
                                    , role_ "tablist"
                                    ]
                                    [ button_
                                        [ tabindex_ "0"
                                        , aria_ "selected" "true"
                                        , aria_ "controls" "demo-tabs-with-panels-panel-1"
                                        , id_ "demo-tabs-with-panels-tab-1"
                                        , role_ "tab"
                                        , type_ "button"
                                        ]
                                        ["Account"]
                                    , button_
                                        [ tabindex_ "0"
                                        , aria_ "selected" "false"
                                        , aria_ "controls" "demo-tabs-with-panels-panel-2"
                                        , id_ "demo-tabs-with-panels-tab-2"
                                        , role_ "tab"
                                        , type_ "button"
                                        ]
                                        ["Password"]
                                    ]
                                , div_
                                    [ aria_ "selected" "true"
                                    , tabindex_ "-1"
                                    , aria_ "labelledby" "demo-tabs-with-panels-tab-1"
                                    , id_ "demo-tabs-with-panels-panel-1"
                                    , role_ "tabpanel"
                                    ]
                                    [ div_
                                        [class_ "card"]
                                        [ header_
                                            []
                                            [ h2_ [] ["Account"]
                                            , p_
                                                []
                                                [ "Make changes to your account here. Click save when you're done."
                                                ]
                                            ]
                                        , section_
                                            []
                                            [ form_
                                                [class_ "form grid gap-6"]
                                                [ div_
                                                    [class_ "grid gap-3"]
                                                    [ label_ [for_ "demo-tabs-account-name"] ["Name"]
                                                    , input_
                                                        [ value_ "Pedro Duarte"
                                                        , id_ "demo-tabs-account-name"
                                                        , type_ "text"
                                                        ]
                                                    ]
                                                , div_
                                                    [class_ "grid gap-3"]
                                                    [ label_
                                                        [for_ "demo-tabs-account-username"]
                                                        ["Username"]
                                                    , input_
                                                        [ value_ "@peduarte"
                                                        , id_ "demo-tabs-account-username"
                                                        , type_ "text"
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        , footer_
                                            []
                                            [ button_
                                                [class_ "btn", type_ "button"]
                                                ["Save changes"]
                                            ]
                                        ]
                                    ]
                                , div_
                                    [ textProp "hidden" ""
                                    , aria_ "selected" "false"
                                    , tabindex_ "-1"
                                    , aria_ "labelledby" "demo-tabs-with-panels-tab-2"
                                    , id_ "demo-tabs-with-panels-panel-2"
                                    , role_ "tabpanel"
                                    ]
                                    [ div_
                                        [class_ "card"]
                                        [ header_
                                            []
                                            [ h2_ [] ["Password"]
                                            , p_
                                                []
                                                [ "Change your password here. After saving, you'll be logged out."
                                                ]
                                            ]
                                        , section_
                                            []
                                            [ form_
                                                [class_ "form grid gap-6"]
                                                [ div_
                                                    [class_ "grid gap-3"]
                                                    [ label_
                                                        [for_ "demo-tabs-password-current"]
                                                        ["Current password"]
                                                    , input_
                                                        [ id_ "demo-tabs-password-current"
                                                        , type_ "password"
                                                        ]
                                                    ]
                                                , div_
                                                    [class_ "grid gap-3"]
                                                    [ label_
                                                        [for_ "demo-tabs-password-new"]
                                                        ["New password"]
                                                    , input_
                                                        [ id_ "demo-tabs-password-new"
                                                        , type_ "password"
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        , footer_
                                            []
                                            [ button_
                                                [class_ "btn", type_ "button"]
                                                ["Save Password"]
                                            ]
                                        ]
                                    ]
                                ]
                            , div_
                                [ data_ "tabs-initialized" "true"
                                , id_ "demo-tabs-without-panels"
                                , class_ "tabs "
                                ]
                                [ nav_
                                    [ aria_ "orientation" "horizontal"
                                    , role_ "tablist"
                                    ]
                                    [ button_
                                        [ tabindex_ "0"
                                        , aria_ "selected" "true"
                                        , aria_
                                            "controls"
                                            "demo-tabs-without-panels-panel-1"
                                        , id_ "demo-tabs-without-panels-tab-1"
                                        , role_ "tab"
                                        , type_ "button"
                                        ]
                                        ["Home"]
                                    , button_
                                        [ tabindex_ "0"
                                        , aria_ "selected" "false"
                                        , aria_
                                            "controls"
                                            "demo-tabs-without-panels-panel-2"
                                        , id_ "demo-tabs-without-panels-tab-2"
                                        , role_ "tab"
                                        , type_ "button"
                                        ]
                                        ["Settings"]
                                    ]
                                ]
                            , div_
                                [ data_ "tabs-initialized" "true"
                                , id_ "demo-tabs-disabled"
                                , class_ "tabs "
                                ]
                                [ nav_
                                    [ aria_ "orientation" "horizontal"
                                    , role_ "tablist"
                                    ]
                                    [ button_
                                        [ tabindex_ "0"
                                        , aria_ "selected" "true"
                                        , aria_ "controls" "demo-tabs-disabled-panel-1"
                                        , id_ "demo-tabs-disabled-tab-1"
                                        , role_ "tab"
                                        , type_ "button"
                                        ]
                                        ["Home"]
                                    , button_
                                        [ disabled_
                                        , tabindex_ "0"
                                        , aria_ "selected" "false"
                                        , aria_ "controls" "demo-tabs-disabled-panel-2"
                                        , id_ "demo-tabs-disabled-tab-2"
                                        , role_ "tab"
                                        , type_ "button"
                                        ]
                                        ["Disabled"]
                                    ]
                                ]
                            , div_
                                [ data_ "tabs-initialized" "true"
                                , id_ "demo-tabs-with-icons"
                                , class_ "tabs "
                                ]
                                [ nav_
                                    [ aria_ "orientation" "horizontal"
                                    , role_ "tablist"
                                    ]
                                    [ button_
                                        [ tabindex_ "0"
                                        , aria_ "selected" "true"
                                        , aria_ "controls" "demo-tabs-with-icons-panel-1"
                                        , id_ "demo-tabs-with-icons-tab-1"
                                        , role_ "tab"
                                        , type_ "button"
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
                                                [ rx_ "2"
                                                , height_ "16"
                                                , width_ "20"
                                                , y_ "4"
                                                , x_ "2"
                                                ]
                                            , path_ [d_ "M10 4v4"]
                                            , path_ [d_ "M2 8h20"]
                                            , path_ [d_ "M6 4v4"]
                                            ]
                                        , "Preview"
                                        ]
                                    , button_
                                        [ tabindex_ "0"
                                        , aria_ "selected" "false"
                                        , aria_ "controls" "demo-tabs-with-icons-panel-2"
                                        , id_ "demo-tabs-with-icons-tab-2"
                                        , role_ "tab"
                                        , type_ "button"
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
                                            [ polyline_ [points_ "16 18 22 12 16 6"]
                                            , polyline_ [points_ "8 6 2 12 8 18"]
                                            ]
                                        , "Code"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "textarea"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Textarea"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/textarea"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-col gap-y-10"]
                            [ textarea_
                                [ placeholder_ "Type your message here"
                                , class_ "textarea"
                                ]
                                []
                            , textarea_
                                [ aria_ "invalid" "true"
                                , placeholder_ "Type your message here"
                                , class_ "textarea"
                                ]
                                []
                            , div_
                                [class_ "grid gap-3"]
                                [ label_
                                    [class_ "label", for_ "textarea-demo-label"]
                                    ["Label"]
                                , textarea_
                                    [ placeholder_ "Type your message here"
                                    , class_ "textarea"
                                    , id_ "textarea-demo-label"
                                    ]
                                    []
                                ]
                            , div_
                                [class_ "grid gap-3"]
                                [ label_
                                    [ class_ "label"
                                    , for_ "textarea-demo-label-and-description"
                                    ]
                                    ["With label and description"]
                                , textarea_
                                    [ placeholder_ "Type your message here"
                                    , class_ "textarea"
                                    , id_ "textarea-demo-label-and-description"
                                    ]
                                    []
                                , p_
                                    [class_ "text-muted-foreground text-sm"]
                                    ["Type your message and press enter to send."]
                                ]
                            , div_
                                [class_ "grid gap-3"]
                                [ label_
                                    [class_ "label", for_ "textarea-demo-disabled"]
                                    ["Disabled"]
                                , textarea_
                                    [ textProp "disabled" ""
                                    , placeholder_ "Type your message here"
                                    , class_ "textarea"
                                    , id_ "textarea-demo-disabled"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "toast"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Toast"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/toast"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-wrap items-center gap-2"]
                            [ div_
                                [class_ "flex flex-wrap items-center gap-2"]
                                [ button_
                                    [ textProp "hx-swap" "beforeend"
                                    , textProp "hx-target" "#toaster"
                                    , textProp "hx-get" "/fragments/toast/success"
                                    , textProp "hx-trigger" "click"
                                    , class_ "btn-outline"
                                    ]
                                    ["Success"]
                                , button_
                                    [ textProp "hx-swap" "beforeend"
                                    , textProp "hx-target" "#toaster"
                                    , textProp "hx-get" "/fragments/toast/error"
                                    , textProp "hx-trigger" "click"
                                    , class_ "btn-outline"
                                    ]
                                    ["Error"]
                                , button_
                                    [ textProp "hx-swap" "beforeend"
                                    , textProp "hx-target" "#toaster"
                                    , textProp "hx-get" "/fragments/toast/info"
                                    , textProp "hx-trigger" "click"
                                    , class_ "btn-outline"
                                    ]
                                    ["Info"]
                                , button_
                                    [ textProp "hx-swap" "beforeend"
                                    , textProp "hx-target" "#toaster"
                                    , textProp "hx-get" "/fragments/toast/warning"
                                    , textProp "hx-trigger" "click"
                                    , class_ "btn-outline"
                                    ]
                                    ["Warning"]
                                ]
                            ]
                        ]
                    ]
                , section_
                    [ class_ "w-full rounded-lg border scroll-mt-14"
                    , id_ "tooltip"
                    ]
                    [ header_
                        [ class_
                            "border-b px-4 py-3 flex items-center justify-between"
                        ]
                        [ h2_ [class_ "text-sm font-medium"] ["Tooltip"]
                        , a_
                            [ data_ "side" "left"
                            , data_ "tooltip" "See documentation"
                            , class_
                                "text-muted-foreground hover:text-foreground"
                            , href_ "/components/tooltip"
                            ]
                            [ svg_
                                [ class_ "size-4"
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
                                [ path_ [d_ "M12 7v14"]
                                , path_
                                    [ d_
                                        "M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z"
                                    ]
                                ]
                            ]
                        ]
                    , div_
                        [class_ "p-4"]
                        [ div_
                            [class_ "flex flex-wrap items-center gap-4"]
                            [ button_
                                [ data_ "tooltip" "Top tooltip"
                                , class_ "btn-outline"
                                , type_ "button"
                                ]
                                ["Top"]
                            , button_
                                [ data_ "side" "right"
                                , data_ "tooltip" "Right tooltip"
                                , class_ "btn-outline"
                                , type_ "button"
                                ]
                                ["Right"]
                            , button_
                                [ data_ "side" "bottom"
                                , data_ "tooltip" "Bottom tooltip"
                                , class_ "btn-outline"
                                , type_ "button"
                                ]
                                ["Bottom"]
                            , button_
                                [ data_ "side" "left"
                                , data_ "tooltip" "Left tooltip"
                                , class_ "btn-outline"
                                , type_ "button"
                                ]
                                ["Left"]
                            ]
                        ]
                    ]
                ]
            ]
        , div_
            [ class_
                "hidden text-sm xl:block w-full max-w-[200px]"
            ]
            [ nav_
                [ class_
                    "sticky top-22 space-y-2 [&_ul]:m-0 [&_ul]:list-none [&_ul_ul]:pl-4 [&_li]:mt-0 [&_li]:pt-2 [&_a]:inline-block [&_a]:no-underline [&_a]:transition-colors [&_a]:hover:text-foreground [&_a]:text-muted-foreground"
                ]
                [ h4_ [class_ "font-medium"] ["On This Page"]
                , ul_
                    []
                    [ li_ [] [a_ [href_ "#accordion"] ["Accordion"]]
                    , li_ [] [a_ [href_ "#alert"] ["Alert"]]
                    , li_
                        []
                        [a_ [href_ "#alert-dialog"] ["Alert Dialog"]]
                    , li_ [] [a_ [href_ "#avatar"] ["Avatar"]]
                    , li_ [] [a_ [href_ "#badge"] ["Badge"]]
                    , li_ [] [a_ [href_ "#breadcrumb"] ["Breadcrumb"]]
                    , li_ [] [a_ [href_ "#button"] ["Button"]]
                    , li_ [] [a_ [href_ "#card"] ["Card"]]
                    , li_ [] [a_ [href_ "#checkbox"] ["Checkbox"]]
                    , li_ [] [a_ [href_ "#combobox"] ["Combobox"]]
                    , li_ [] [a_ [href_ "#dialog"] ["Dialog"]]
                    , li_
                        []
                        [a_ [href_ "#dropdown-menu"] ["Dropdown Menu"]]
                    , li_ [] [a_ [href_ "#form"] ["Form"]]
                    , li_ [] [a_ [href_ "#input"] ["Input"]]
                    , li_ [] [a_ [href_ "#label"] ["Label"]]
                    , li_ [] [a_ [href_ "#pagination"] ["Pagination"]]
                    , li_ [] [a_ [href_ "#popover"] ["Popover"]]
                    , li_ [] [a_ [href_ "#radio-group"] ["Radio Group"]]
                    , li_ [] [a_ [href_ "#select"] ["Select"]]
                    , li_ [] [a_ [href_ "#skeleton"] ["Skeleton"]]
                    , li_ [] [a_ [href_ "#slider"] ["Slider"]]
                    , li_ [] [a_ [href_ "#switch"] ["Switch"]]
                    , li_ [] [a_ [href_ "#table"] ["Table"]]
                    , li_ [] [a_ [href_ "#tabs"] ["Tabs"]]
                    , li_ [] [a_ [href_ "#textarea"] ["Textarea"]]
                    , li_ [] [a_ [href_ "#toast"] ["Toast"]]
                    , li_ [] [a_ [href_ "#tooltip"] ["Tooltip"]]
                    ]
                ]
            ]
        ]
    ]
   
