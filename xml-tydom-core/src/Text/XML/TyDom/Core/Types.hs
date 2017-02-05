{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-|
Module      : Text.XML.TyDom.Core.Types
Description : Classes and types parameterized over a very generic XML DOM.
Copyright   : (c) Jonathan Merritt 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : Experimental
Portability : POSIX

This module contains classes and types which are parameterized to allow a very
generic typed DOM interface.

Type parameters have been abbreviated to single letters, which are used
uniformly throughout:

* @e@ - element type
* @n@ - type for the name of an element
* @a@ - type for the name of an attribute
* @t@ - type for text content in the XML document
-}
module Text.XML.TyDom.Core.Types
    ( -- * Classes
      ToElem (toElem)
    , FromElem (fromElem)
    , ToXText (toXText)
    , FromXText (fromXText)
    , Conv (conv)
      -- * XML DOM Types
    , Attr (Attr, unAttr)
    , Child (Child, unChild)
    , Content (Content, unContent)
    , CData (CData, unCData)
      -- * Result Types
    , XTextError (XTextError, unXTextError)
    , Result (Success, Failure)
    , Path (PathItem, PathRoot)
    , Cause (..)
      -- * Functions
    , xTextErrType
    , prependPath
    , replacePathHead
    ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Data.Text           (Text)
import qualified Data.Text           as Text (concat)
import           Test.QuickCheck     (Arbitrary)

-------------------------------------------------------------------------------
-- Classes

-- | Typeclass for a type @z@ that can be represented as element type @e@.
class ToElem e z where
    -- | Converts a value of type @z@ to element type @e@.
    toElem :: z -> e

-- | Typeclass for a type @z@ which can be read from an element of type @e@.
class FromElem e n a t z where
    -- | Converts a value of element type @e@ to a @Result@ of type @z@, thus
    --   allowing for the possibility of failure.
    fromElem :: e -> Result e n a t z

-- | Typeclass for a type @z@ which can be represented as text type @t@.
class ToXText t z where
    -- | Converts a value of type @z@ to text type @t@.
    toXText :: z -> t

-- | Typeclass for a type @z@ which can be read from a text type @t@.
class FromXText t z where
    -- | Reads a value of type @t@ into an @Either@.
    --
    --   As a failure case (@Left@), the @Either@ may contain an @XTextError@,
    --   describing the reason for the failure. On success (@Right@), the
    --   @Either@ contains a value of type @z@.
    fromXText :: t -> Either XTextError z

-- | Typeclass for conversion between types @p@ and @q@.
--
--   This typeclass is particularly used for conversion between raw types and
--   their XML-decorated versions, corresponding to 'Attr', 'Child', 'Content'
--   and 'CData'.
class Conv p q where
    -- | Converts a value of type @p@ to a value of type @q@.
    conv :: p -> q

-------------------------------------------------------------------------------
-- Parts of the XML Dom

-- | Attribute.
--
--   Specifies that a record field of type @Attr z@ should become an XML
--   attribute. The name of the attribute is specified by the name of the record
--   selector, while the value is the textual representation of the value of
--   type @z@.
newtype Attr z = Attr { unAttr :: z } deriving (Eq, Show, Arbitrary)

-- | Child (containing only text).
--
--   Specifies that a record field of type @Child z@ should become a child
--   element of the current element, containing the textual representation of
--   the value of type @z@.
newtype Child z = Child { unChild :: z } deriving (Eq, Show, Arbitrary)

-- | Content node.
--
--   Specifies that a record field of type @Content z@ should become a content
--   node of the current element, containing the textual representation of the
--   value of type @z@.
newtype Content z = Content { unContent :: z } deriving (Eq, Show, Arbitrary)

-- | CData node.
--
--   Specifies that a record field of type @CData z@ should become a CDATA node
--   of the current element, containing the textual representation of the value
--   of type @z@.
newtype CData z = CData { unCData :: z } deriving (Eq, Show, Arbitrary)

-------------------------------------------------------------------------------
-- Conv instances

instance Conv z z where conv = id

instance Conv z (Attr    z) where conv = Attr
instance Conv z (Child   z) where conv = Child
instance Conv z (Content z) where conv = Content
instance Conv z (CData   z) where conv = CData

instance Conv (Attr    z) z where conv = unAttr
instance Conv (Child   z) z where conv = unChild
instance Conv (Content z) z where conv = unContent
instance Conv (CData   z) z where conv = unCData

-------------------------------------------------------------------------------
-- Result types 
                                  
-- | Error which may occur when parsing XML text.
newtype XTextError = XTextError { unXTextError :: Text } deriving (Eq, Show)

-- | Result of converting an element (type @e@) to type @z@.
data Result e n a t z
    = -- | Successful result of type @z@.
      Success z
      -- | Failure.
      --
      --   The failure contains the 'Path' to the element which failed, as well
      --   as the 'Cause' of the failure.
    | Failure (Path n) (Cause e n a t)
    deriving (Functor)

instance Applicative (Result e n a t) where
    pure = Success
    Success f   <*> Success a   = Success (f a)
    _           <*> Failure p c = Failure p c
    Failure p c <*> _           = Failure p c

instance Alternative (Result elem elemN attrN txt) where
    empty = Failure PathRoot (Cause "Empty Result alternative!")
    Success v   <|> _           = Success v
    Failure _ _ <|> Success v   = Success v
    Failure _ _ <|> Failure p c = Failure p c

instance Monad (Result elem elemN attrN txt) where
    Success x   >>= f = f x
    Failure p c >>= _ = Failure p c

-- | Path to a failure.
data Path n = -- | Item in the path.
              PathItem n (Path n)
              -- | Root (source / origin) of the path.
            | PathRoot

-- | Cause of a failure.
data Cause e n a t
    = Cause Text
    | Leftover e
    | WrongElementName
    { expectedElementName :: n
    , actualElementName   :: n }
    | MissingAttribute a
    | FailParseAttribute a XTextError
    | FailParseContent XTextError
    | FailParseCData XTextError
    | MissingElement n
    | NoMoreElements
    | MissingContent
    | MissingCData
    | NoSuccessfulChildren

deriving instance Show n => Show (Path n)
deriving instance (Show e, Show n, Show a, Show t) => Show (Cause e n a t)
deriving instance (Show e, Show n, Show a, Show t, Show z)
                  => Show (Result e n a t z)

-- | Formats an 'XTextError' string and returns it as a 'Left' instance.
xTextErrType :: Text -> Text -> Either XTextError a
xTextErrType typeName input = Left $ XTextError $ Text.concat
                              [ "Could not read ["
                              , input
                              , "] as type "
                              , typeName ]

-- | Prepends an element name to a 'Path' in a failure result.
prependPath :: n  -> Result e n a t z -> Result e n a t z
prependPath _ s@(Success _) = s
prependPath n (Failure p c) = Failure (PathItem n p) c

-- | Replaces the current head of a 'Path' in a failure result.
replacePathHead :: n -> Result e n a t z -> Result e n a t z
replacePathHead _ s@(Success _)              = s
replacePathHead _ f@(Failure PathRoot _)     = f
replacePathHead n (Failure (PathItem _ p) c) = Failure (PathItem n p) c
