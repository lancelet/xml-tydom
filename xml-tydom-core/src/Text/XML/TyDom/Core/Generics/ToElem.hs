{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : Text.XML.TyDom.Core.Generics.ToElem
Description : Generic producers for ToElem instances.
Copyright   : (c) Jonathan Merritt 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : Experimental
Portability : POSIX
-}
module Text.XML.TyDom.Core.Generics.ToElem
    ( GToElem
    , genericToElem
    ) where

import Text.XML.TyDom.Core.Generics.Types (OptionsElement (optAttrName,
                                                           optConstructorElemName,
                                                           optSelectorElemName),
                                           symt)
import Text.XML.TyDom.Core.Types          (Attr (Attr), CData (CData),
                                           Child (Child), Content (Content),
                                           ToElem (toElem), ToXText (toXText))
import Text.XML.TyDom.Core.XMLInterface   (Compose, cAttr, cCData, cChild,
                                           cContent, cEmpty, cFreeze, cName,
                                           cThaw, cNull)

import Data.List    (foldl')
import Data.Proxy   (Proxy (Proxy))
import GHC.Generics
import GHC.TypeLits (KnownSymbol)

-------------------------------------------------------------------------------

-- | Generic producer for a 'ToElem' instance.
genericToElem :: (Generic z, GToElem e n a t (Rep z))
              => OptionsElement n a
              -> Compose e n a t c
              -> (z -> e)
genericToElem o c z = cFreeze c $ gToElem o c (from z) $ cEmpty c

-------------------------------------------------------------------------------

-- | Class for generically converting a type to an element.
class GToElem e n a t z where
    gToElem :: OptionsElement n a -> Compose e n a t c -> z r -> (c -> c)

------ Datatype and constructors

-- | D1 - Datatype (non-newtype).
--
--   When we encounter a Datatype, just proceed directly to processing its
--   contents, without any additional handling.
instance GToElem e n a t z =>
         GToElem e n a t (D1 ('MetaData g h i 'False) z) where
    gToElem o c (M1 z) = gToElem o c z

-- | Newtype.
--
--   A newtype should be processed in the same way as the type it wraps, but
--   the name of the element must be changed at the end.
instance (KnownSymbol name, ToElem e z) =>
         GToElem e n a t (D1 ('MetaData g h i 'True)
                              (C1 ('MetaCons name q w)
                                  (S1 s (Rec0 z)))) where
    gToElem o c (M1 (M1 (M1 (K1 z)))) = setCreatedElemName o c p (toElem z)
      where p = Proxy :: Proxy name

-- | U1 - no-argument constructor.
instance GToElem e n a t U1 where
    gToElem _ _ _ = id

-- | C1 - constructor.
--
--   The name of the element is obtained from the constructor name.
instance (KnownSymbol name, GToElem e n a t z) =>
         GToElem e n a t (C1 ('MetaCons name q w) z) where
    gToElem o c (M1 z) = setElemName o c p . gToElem o c z
      where p = Proxy :: Proxy name

------ Sums and products

-- | Product type (ie. multiple fields).
instance (GToElem e n a t z1, GToElem e n a t z2) =>
         GToElem e n a t (z1 :*: z2) where
    gToElem o c (z1 :*: z2) = gToElem o c z2 . gToElem o c z1

-- | Sum type (ie. multiple constructors).
instance (GToElem e n a t z1, GToElem e n a t z2) =>
         GToElem e n a t (z1 :+: z2) where
    gToElem o c (L1 z1) = gToElem o c z1
    gToElem o c (R1 z2) = gToElem o c z2

------ Attr

-- | S1 (named) + Attr - record selector for an XML attribute.
instance {-# OVERLAPS #-} (KnownSymbol name, ToXText t z) =>
         GToElem e n a t (S1 ('MetaSel ('Just name) g h i)
                             (Rec0 (Attr z))) where
    gToElem o c (M1 (K1 (Attr z))) = setAttr o c p z
      where p = Proxy :: Proxy name

-- | S1 (named) + Attr Maybe - record selector for optional XML attribute.
instance {-# OVERLAPS #-} (KnownSymbol name, ToXText t z) =>
         GToElem e n a t (S1 ('MetaSel ('Just name) g h i)
                             (Rec0 (Attr (Maybe z)))) where
    gToElem _ _ (M1 (K1 (Attr Nothing))) = id
    gToElem o c (M1 (K1 (Attr (Just z)))) = setAttr o c p z
      where p = Proxy :: Proxy name

------ Child

-- | S1 (named) + Child - record selector for a simple child element with text.
instance {-# OVERLAPS #-} (KnownSymbol name, ToXText t z) =>
         GToElem e n a t (S1 ('MetaSel ('Just name) g h i)
                             (Rec0 (Child z))) where
    gToElem o c (M1 (K1 (Child z))) = addTextChild o c p z
      where p = Proxy :: Proxy name

-- | S1 (named) + Child Maybe - record selector for an optional simple child
--   element with text.
instance {-# OVERLAPS #-} (KnownSymbol name, ToXText t z) =>
         GToElem e n a t (S1 ('MetaSel ('Just name) g h i)
                             (Rec0 (Child (Maybe z)))) where
    gToElem _ _ (M1 (K1 (Child Nothing))) = id
    gToElem o c (M1 (K1 (Child (Just z)))) = addTextChild o c p z
      where p = Proxy :: Proxy name

-- | S1 (named) + [Child] - record selector for a list of simple child elements
--   with text.
instance {-# OVERLAPS #-} (KnownSymbol name, ToXText t z) =>
         GToElem e n a t (S1 ('MetaSel ('Just name) g h i)
                             (Rec0 (Child [z]))) where
    gToElem o c (M1 (K1 (Child zs))) = appRList (addTextChild o c p <$> zs)
      where p = Proxy :: Proxy name

------ Content

-- | S1 (named or unnamed) + Content - record selector for a content node.
instance ToXText t z => GToElem e n a t (S1 q (Rec0 (Content z))) where
    gToElem _ c (M1 (K1 (Content z))) = addContent c z

-- | S1 (named or unnamed) + Content Maybe - record selector for an optional
--   content node.
instance {-# OVERLAPS #-} ToXText t z =>
         GToElem e n a t (S1 q (Rec0 (Content (Maybe z)))) where
    gToElem _ _ (M1 (K1 (Content Nothing)))  = id
    gToElem _ c (M1 (K1 (Content (Just z)))) = addContent c z

------ CData

-- | S1 (named or unnamed) + CData - record selector for a CDATA child node.
instance ToXText t z => GToElem e n a t (S1 q (Rec0 (CData z))) where
    gToElem _ c (M1 (K1 (CData z))) = addCData c z

-- | S1 (named or unnamed) + CData Maybe - record selector for an optional CDATA
--   child node.
instance ToXText t z => GToElem e n a t (S1 q (Rec0 (CData (Maybe z)))) where
    gToElem _ _ (M1 (K1 (CData Nothing)))  = id
    gToElem _ c (M1 (K1 (CData (Just z)))) = addCData c z

-- | S1 (named or unnamed) + [CData] - record selector for a list of CDATA child
--   nodes.
instance ToXText t z => GToElem e n a t (S1 q (Rec0 (CData [z]))) where
    gToElem _ c (M1 (K1 (CData zs))) = appRList (addCData c <$> zs)

------ ToElem children

-- | S1 (named or unnamed) - record selector for a ToElem child.
instance {-# OVERLAPS #-} ToElem e z => GToElem e n a t (S1 q (Rec0 z)) where
    gToElem _ c (M1 (K1 z)) = addElemChild c z

-- | S1 (named or unnamed) + Maybe z - record selector for a ToElem child.
instance ToElem e z => GToElem e n a t (S1 q (Rec0 (Maybe z))) where
    gToElem _ _ (M1 (K1 Nothing))  = id
    gToElem _ c (M1 (K1 (Just z))) = addElemChild c z

-- | S1 (named or unnamed) + [z] - record selector for a list of ToElem child
--   nodes.
instance {-# OVERLAPS #-} ToElem e z => GToElem e n a t (S1 q (Rec0 [z])) where
    gToElem _ c (M1 (K1 zs)) = appRList (addElemChild c <$> zs)

-------------------------------------------------------------------------------
-- Helper functions internal to this module.

-- | Sets the name of a 'Compose' element of type @c@.
setElemName :: (KnownSymbol name)
            => OptionsElement n a
            -> Compose e n a t c
            -> Proxy name
            -> (c -> c)
setElemName o c p = cName c (optConstructorElemName o (symt p))

-- | Sets the name of a final element of type @e@.
setCreatedElemName :: (KnownSymbol name)
                   => OptionsElement n a
                   -> Compose e n a t c
                   -> Proxy name
                   -> e
                   -> (c -> c)
setCreatedElemName o c p = const . setElemName o c p . cThaw c

-- | Sets an attribute on a 'Compose' element of type @c@.
setAttr :: (KnownSymbol name, ToXText t v)
        => OptionsElement n a
        -> Compose e n a t c
        -> Proxy name
        -> v
        -> (c -> c)
setAttr o c p v = cAttr c (optAttrName o (symt p)) (toXText v)


-- | Adds a child with text-only content to a 'Compose' element of type @c@.
addTextChild :: (KnownSymbol name, ToXText t v)
             => OptionsElement n a
             -> Compose e n a t c
             -> Proxy name
             -> v
             -> (c -> c)
addTextChild o c p v = cChild c
                       ( cFreeze c
                       $ cContent c (toXText v)
                       $ cName c (optSelectorElemName o (symt p))
                       $ cEmpty c )

-- | Adds a content node to a 'Compose' element of type @c@.
addContent :: ToXText t v => Compose e n a t c -> v -> (c -> c)
addContent c v =
    let text = toXText v
    in if cNull c text
       then id
       else cContent c (toXText v)

-- | Adds a CDATA child node to a 'Compose' element of type @c@.
addCData :: ToXText t v => Compose e n a t c -> v -> (c -> c)
addCData c v = cCData c (toXText v)

-- | Applies a list of functions one at a time, in reverse, to produce a final
--   function.
appRList :: [c -> c] -> (c -> c)
appRList fs = foldl' (.) id (reverse fs)

-- | Adds a ToElem child element to a 'Compose' element of type @c@.
addElemChild :: ToElem e z => Compose e n a t c -> z -> (c -> c)
addElemChild c z = cChild c (toElem z)
