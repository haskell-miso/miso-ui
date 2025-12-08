-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultilineStrings           #-}
-----------------------------------------------------------------------------
module Miso.UI.Avatar
  ( -- ** Views
    avatar_
    -- ** Samples
  , avatarSample
  , avatarCodeSample
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
avatar_
  :: [Attribute action]
  -> View model action
avatar_ attrs =
  H.img_ (attrs ++ [ P.class_ "size-8 shrink-0 object-cover rounded-full" ])
-----------------------------------------------------------------------------
avatarSample :: View model action
avatarSample =
  H.div_
  [ P.class_ "flex flex-row flex-wrap items-center gap-4" ]
  [ avatar_
      [ P.src_ "https://github.com/dmjio.png"
      , P.alt_ "@dmjio"
      ]
  , H.span_
      [ P.class_ "size-8 shrink-0 bg-muted flex items-center justify-center rounded-full" ]
      [ "CN" ]
  , avatar_
      [ P.src_ "https://github.com/dmjio.png"
      , P.alt_ "@dmjio"
      , P.class_ "size-12 shrink-0 object-cover rounded-full"
      ]
  , avatar_
      [ P.src_ "https://github.com/dmjio.png"
      , P.alt_ "@dmjio"
      , P.class_ "size-8 shrink-0 object-cover rounded-lg"
      ]
  , H.div_
      [ P.class_ "flex -space-x-2 [&_img]:ring-background [&_img]:ring-2 [&_img]:grayscale [&_img]:size-8 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full" ]
      [ H.img_ [ P.src_ "https://github.com/dmjio.png", P.alt_ "@dmjio" ]
      , H.img_ [ P.src_ "https://github.com/shadcn.png", P.alt_ "@shadcn" ]
      , H.img_ [ P.src_ "https://github.com/adamwathan.png", P.alt_ "@adamwathan" ]
      , H.img_ [ P.src_ "https://github.com/hunvreus.png", P.alt_ "@hunvreus" ]
      ]
  , H.div_
      [ P.class_ "flex -space-x-2 [&_img]:ring-background [&_img]:ring-2 [&_img]:grayscale [&_img]:size-12 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full" ]
      [ H.img_ [ P.src_ "https://github.com/dmjio.png", P.alt_ "@dmjio" ]
      , H.img_ [ P.src_ "https://github.com/shadcn.png", P.alt_ "@shadcn" ]
      , H.img_ [ P.src_ "https://github.com/adamwathan.png", P.alt_ "@adamwathan" ]
      , H.img_ [ P.src_ "https://github.com/hunvreus.png", P.alt_ "@hunvreus" ]
      ]
  , H.div_
      [ P.class_ "flex -space-x-2 hover:space-x-1 [&_img]:ring-background [&_img]:size-12 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full [&_img]:ring-2 [&_img]:grayscale [&_img]:transition-all [&_img]:ease-in-out [&_img]:duration-300" ]
      [ H.img_ [ P.src_ "https://github.com/dmjio.png", P.alt_ "@dmjio" ]
      , H.img_ [ P.src_ "https://github.com/shadcn.png", P.alt_ "@shadcn" ]
      , H.img_ [ P.src_ "https://github.com/adamwathan.png", P.alt_ "@adamwathan" ]
      , H.img_ [ P.src_ "https://github.com/hunvreus.png", P.alt_ "@hunvreus" ]
      ]
  ]
-----------------------------------------------------------------------------
avatarCodeSample :: View model action
avatarCodeSample =
  """
  -----------------------------------------------------------------------------
  module MyAvatar (avatarSample) where
  -----------------------------------------------------------------------------
  import           Miso
  import qualified Miso.Html as H
  import qualified Miso.Html.Property as P
  -----------------------------------------------------------------------------
  import qualified Miso.UI.Avatar as Avatar
  -----------------------------------------------------------------------------
  avatarSample :: View model action
  avatarSample =
    H.div_
    [ P.class_ "flex flex-row flex-wrap items-center gap-4" ]
    [ Avatar.avatar_
        [ P.src_ "https://github.com/dmjio.png"
        , P.alt_ "@dmjio"
        ]
    , H.span_
        [ P.class_ "size-8 shrink-0 bg-muted flex items-center justify-center rounded-full" ]
        [ "CN" ]
    , Avatar.avatar_
        [ P.src_ "https://github.com/dmjio.png"
        , P.alt_ "@dmjio"
        , P.class_ "size-12 shrink-0 object-cover rounded-full"
        ]
    , Avatar.avatar_
        [ P.src_ "https://github.com/dmjio.png"
        , P.alt_ "@dmjio"
        , P.class_ "size-8 shrink-0 object-cover rounded-lg"
        ]
    , H.div_
        [ P.class_ "flex -space-x-2 [&_img]:ring-background [&_img]:ring-2 [&_img]:grayscale [&_img]:size-8 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full" ]
        [ H.img_ [ P.src_ "https://github.com/dmjio.png", P.alt_ "@dmjio" ]
        , H.img_ [ P.src_ "https://github.com/shadcn.png", P.alt_ "@shadcn" ]
        , H.img_ [ P.src_ "https://github.com/adamwathan.png", P.alt_ "@adamwathan" ]
        , H.img_ [ P.src_ "https://github.com/hunvreus.png", P.alt_ "@hunvreus" ]
        ]
    , H.div_
        [ P.class_ "flex -space-x-2 [&_img]:ring-background [&_img]:ring-2 [&_img]:grayscale [&_img]:size-12 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full" ]
        [ H.img_ [ P.src_ "https://github.com/dmjio.png", P.alt_ "@dmjio" ]
        , H.img_ [ P.src_ "https://github.com/shadcn.png", P.alt_ "@shadcn" ]
        , H.img_ [ P.src_ "https://github.com/adamwathan.png", P.alt_ "@adamwathan" ]
        , H.img_ [ P.src_ "https://github.com/hunvreus.png", P.alt_ "@hunvreus" ]
        ]
    , H.div_
        [ P.class_ "flex -space-x-2 hover:space-x-1 [&_img]:ring-background [&_img]:size-12 [&_img]:shrink-0 [&_img]:object-cover [&_img]:rounded-full [&_img]:ring-2 [&_img]:grayscale [&_img]:transition-all [&_img]:ease-in-out [&_img]:duration-300" ]
        [ H.img_ [ P.src_ "https://github.com/dmjio.png", P.alt_ "@dmjio" ]
        , H.img_ [ P.src_ "https://github.com/shadcn.png", P.alt_ "@shadcn" ]
        , H.img_ [ P.src_ "https://github.com/adamwathan.png", P.alt_ "@adamwathan" ]
        , H.img_ [ P.src_ "https://github.com/hunvreus.png", P.alt_ "@hunvreus" ]
        ]
    ]
  """
