{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-|
Module      : Text.XML.TyDom.Core.XMLInterface
Description : Abstracts over an underlying DOM representation.
Copyright   : (c) Jonathan Merritt 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : Experimental
Portability : POSIX

This module contains the underlying machinery for the generic implementations of
the classes that read and write XML. Examples of this functionality include the
ability to get or set names of elements, get or set attributes, get or set child
elements, and so on. The 'Compose' and 'Decompose' data types abstract over any
underlying XML DOM representation to provide the required functionality. Thus,
provided 'Compose' and 'Decompose' can be written for a given XML DOM backend,
that backend can be used with @xml-tydom@.

Importantly, the requirements of @xml-tydom@ may differ from those of any given
XML DOM backend. Because of this, 'Compose' and 'Decompose' both have the
concepts of /freeze/ and /thaw/, which convert types in an actual XML DOM
backend to types that can be chosen to be well-suited to @xml-tydom@ itself.
These special element representations are referred to as compose-elements and
decompose-elements.

'Compose' and 'Decompose' are parameterised by a variety of type variables that
describe the underlying API. We have attempted to name these in a uniform
fashion:

  - @e@: type of an element
  - @n@: type of the name of an element
  - @a@: type of the name of an attribute
  - @t@: type of text content in the XML document
  - @c@: type of the temporary representation of a compose-element used by
         the 'Compose' data type
  - @d@: type of the temporary representation of a decompose-element used by
         the 'Decompose' data type
-}
module Text.XML.TyDom.Core.XMLInterface
    ( -- * Summary
      -- $summary

      -- * Types
      Compose (..)
    , Decompose (..)
    , Result (Success, Failure)
    ) where

-------------------------------------------------------------------------------

-- | Composes an element.
data Compose e n a t c
    = Compose
    { -- | Empty instance of a compose-element of type @c@.
      cEmpty    :: c
      -- | Sets the name of a compose-element.
    , cName     :: n -> c -> c
      -- | Adds an attribute to a compose-element.
    , cAttr     :: a -> t -> c -> c
      -- | Adds a child element to a compose-element.
    , cChild    :: e -> c -> c
      -- | Adds a text content node to a compose-element.
    , cContent  :: t -> c -> c
      -- | Adds a CDATA node to a compose-element.
    , cCData    :: t -> c -> c
      -- | Freezes a compose-element into an ordinary element.
    , cFreeze   :: c -> e
      -- | Thaws an ordinary element into a compose-element.
    , cThaw     :: e -> c
      -- | Checks if given text is an empty string.
    , cNull     :: t -> Bool
    }

-------------------------------------------------------------------------------

-- | Decomposes an element.
--
--   An important principle when decomposing an element is that when attributes
--   or child nodes are returned, a new version of the element is also returned
--   /without/ those attributes or elements. This is important for the case
--   where we want to check that an element has been read completely.
data Decompose e n a t d
    = Decompose
    { -- | Thaws an ordinary element into a decompose-element.
      dThaw                :: e -> d
      -- | Freezes a decompose-element into an ordinary element.
    , dFreeze              :: d -> e
      -- | An empty decompose-element.
    , dEmpty               :: d
      -- | Checks if a decompose-element is empty.
    , dNull                :: d -> Bool
      -- | Returns the name of a decompose element.
    , dName                :: d -> n
      -- | Renames a decompose element element.
    , dRename              :: n -> d -> d
      -- | Extracts an attribute.
    , dAttr                :: a -> d -> Result d t
      -- | Next child element, iff the next node is an element.
    , dNextSeqChild        :: d -> Result d d
      -- | Next text content, iff the next node is text.
    , dNextSeqContent      :: d -> Result d t
      -- | Next CDATA content, iff the next node is CDATA.
    , dNextSeqCData        :: d -> Result d t
      -- | Next child element with the given name, even if it is not the next
      --   node.
    , dNextChildNamed      :: n -> d -> Result d d
      -- | Next text content, even if the next node is not text.
    , dNextContent         :: d -> Result d t
      -- | Next CDATA content, even if the next node is not CDATA.
    , dNextCData           :: d -> Result d t
      -- | First child node that succeeds with the supplied function.
    , dSuccessChild        :: forall z. d -> (e -> Maybe z) -> Result d z
      -- | Consumes sequential children with the supplied function until the
      --   element is exhausted, or until one child fails. (Should never fail.)
    , dSuccessNextChildren :: forall z. d -> (e -> Maybe z) -> (d, [z])
      -- | Consumes all children (even non-contiguous ones) that satisfy the
      --   supplied function. (Should never fail.)
    , dSuccessChildren     :: forall z. d -> (e -> Maybe z) -> (d, [z])
      -- | All content from the element (even non-contiguous).
    , dAllContent          :: d -> Result d t
      -- | Consume all contiguous CData nodes. (Should never fail.)
    , dAllNextCData        :: d -> (d, [t])
      -- | Consume add CData (even non-contiguous). (Should never fail.)
    , dAllCData            :: d -> (d, [t])
      -- | Empty text value.
    , dEmptyTxt            :: t
    }

-- | Result type for operations from 'Decompose'.
data Result d z = -- | Successful result. Contains both the result type (@z@)
                  --   and a new version of the decompose-element @d@ without
                  --   the particular thing that was returned (ie. without that
                  --   particular attribute or child node).
                  Success d z
                  -- | Failure case; no additional information is required.
                | Failure

deriving instance (Show d, Show z) => Show (Result d z)
