-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
-----------------------------------------------------------------------------
module Types where
-----------------------------------------------------------------------------
import Miso
import Miso.Router
-----------------------------------------------------------------------------
import GHC.Generics
-----------------------------------------------------------------------------
data Action
  = ToggleDarkMode
  | ChangeTheme MisoString
  | ToggleSidebar
  | Highlight DOMRef
  | InitSlider DOMRef
  | DestroySlider DOMRef
  | Toaster { category, title, description, label :: MisoString }
-----------------------------------------------------------------------------
data Model
  = Model
  { _currentPage :: Page
  } deriving Eq
-----------------------------------------------------------------------------
data Page = Index
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Router)
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model Index
-----------------------------------------------------------------------------
