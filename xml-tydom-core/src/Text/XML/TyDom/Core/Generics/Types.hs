{-|
Module      : Text.XML.TyDom.Core.Generics.Types
Description : Types that configure generic XML instances.
Copyright   : (c) Jonathan Merritt 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : Experimental
Portability : POSIX
-}
module Text.XML.TyDom.Core.Generics.Types
    ( OptionsElement (..)
    , ReadNodeOrdering (..)
    , ReadLeftovers (..)
    , symt
    ) where

import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import GHC.TypeLits (KnownSymbol, symbolVal)

-------------------------------------------------------------------------------
-- Generic configuration options.

-- | Options for generating 'ToElem' and 'FromElem' instances using GHC
--   Generics.
--
--   Type parameters:
--
--   * @n@ - Type for an element name.
--   * @a@ - Type for an attribute name.
data OptionsElement n a
    = OptionsElement
    { -- | Formats an element name from the name of a data type constructor.
      optConstructorElemName :: Text -> n
      -- | Formats an element name from the name of a record selector.
    , optSelectorElemName    :: Text -> n
      -- | Formats an attribute name from the name of a record selector.
    , optAttrName            :: Text -> a
      -- | Specifies the read ordering of children.
    , optReadNodeOrdering    :: ReadNodeOrdering
      -- | When reading, is it OK to have leftover attributes / elements?
    , optReadLeftovers       :: ReadLeftovers
    }

-- | Specifies how child nodes should be treated when reading a type from an
--   element.
data ReadNodeOrdering
    = -- | Child nodes should be read in strict sequence (ie.
      --   @\<xsd:sequence>@).
      Sequence
      -- | Child nodes can appear in any order (ie. @\<xsd:all>@).
    | All
    deriving (Eq, Show)

-- | Specifies how any left-over parts of an element should be treated when
--   reading a type from an element.
data ReadLeftovers
      -- | Left-over parts of an element are OK, and not an error.
    = LeftoversOK
      -- | Left-over parts of an element should produce an error.
    | LeftoversError
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Functions

-- | A @KnownSymbol@ as a @Text@ value.
symt :: (KnownSymbol s) => Proxy s -> Text
symt = T.pack . symbolVal
