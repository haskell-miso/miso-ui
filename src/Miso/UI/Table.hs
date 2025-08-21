-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
-----------------------------------------------------------------------------
module Miso.UI.Table
  ( -- ** Component
    table_
  ) where
-----------------------------------------------------------------------------
import           Miso hiding (table_)
-- import qualified Miso.Svg as S
-- import qualified Miso.Svg.Property as SP
import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Property as P
-- import           Miso.Lens
-----------------------------------------------------------------------------
table_ :: Component parent model action
table_ = component undefined noop (const view_)
-----------------------------------------------------------------------------
view_ :: View model action
view_ = div_
    [ class_ "overflow-x-auto" ]
    [ H.table_
        [ class_ "table" ]
        [ caption_ [][ "A list of your recent invoices." ]
        , thead_ []
            [ tr_ []
                [ th_ [][ "Invoice" ]
                , th_ [][ "Status" ]
                , th_ [][ "Method" ]
                , th_ [][ "Amount" ]
                ]
            ]
        , tbody_ []
            [ tr_ []
                [ td_
                    [ class_ "font-medium" ][ "INV001" ]
                , td_ [][ "Paid" ]
                , td_ [][ "Credit Card" ]
                , td_
                    [ class_ "text-right" ][ "$250.00" ]
                ]
            , tr_ []
                [ td_
                    [ class_ "font-medium" ][ "INV002" ]
                , td_ [][ "Pending" ]
                , td_ [][ "PayPal" ]
                , td_
                    [ class_ "text-right" ][ "$150.00" ]
                ]
            , tr_ []
                [ td_
                    [ class_ "font-medium" ][ "INV003" ]
                , td_ [][ "Unpaid" ]
                , td_ [][ "Bank Transfer" ]
                , td_
                    [ class_ "text-right" ][ "$350.00" ]
                ]
            , tr_ []
                [ td_
                    [ class_ "font-medium" ][ "INV004" ]
                , td_ [][ "Paid" ]
                , td_ [][ "Paypal" ]
                , td_
                    [ class_ "text-right" ][ "$450.00" ]
                ]
            , tr_ []
                [ td_
                    [ class_ "font-medium" ][ "INV005" ]
                , td_ [][ "Paid" ]
                , td_ [][ "Credit Card" ]
                , td_
                    [ class_ "text-right" ][ "$550.00" ]
                ]
            , tr_ []
                [ td_
                    [ class_ "font-medium" ][ "INV006" ]
                , td_ [][ "Pending" ]
                , td_ [][ "Bank Transfer" ]
                , td_
                    [ class_ "text-right" ][ "$200.00" ]
                ]
            , tr_ []
                [ td_
                    [ class_ "font-medium" ][ "INV007" ]
                , td_ [][ "Unpaid" ]
                , td_ [][ "Credit Card" ]
                , td_
                    [ class_ "text-right" ][ "$300.00" ]
                ]
            ]
        , tfoot_ []
            [ tr_ []
                [ td_
                    [ colspan_ "3" ][ "Total" ]
                , td_
                    [ class_ "text-right" ][ "$2,500.00" ]
                ]
            ]
        ]
    ]

