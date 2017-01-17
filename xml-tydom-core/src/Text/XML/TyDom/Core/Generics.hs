-- {-# OPTIONS_GHC -Wwarn #-}

{-|
Module      : Text.XML.TyDom.Core.Generics
Description : Generic implementations of XML reading and writing.
Copyright   : (c) Jonathan Merritt 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : Experimental
Portability : POSIX
-}
module Text.XML.TyDom.Core.Generics
    ( -- * Classes
      GToElem
    , GFromElem
      -- * Types
    , OptionsElement (..)
    , ReadNodeOrdering (..)
    , ReadLeftovers (..)
      -- * Generic instance creation
    , genericToElem
    , genericFromElem
    , genericConv
    ) where

import Text.XML.TyDom.Core.Generics.ToElem (genericToElem, GToElem)
import Text.XML.TyDom.Core.Generics.FromElem (genericFromElem, GFromElem)
import Text.XML.TyDom.Core.Generics.Conv (genericConv)
import Text.XML.TyDom.Core.Generics.Types (OptionsElement (..),
                                           ReadNodeOrdering (..),
                                           ReadLeftovers (..))
