{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : Text.XML.TyDom.Core.Generics.FromElem
Description : Generic producers for FromElem instances.
Copyright   : (c) Jonathan Merritt 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : Experimental
Portability : POSIX
-}
module Text.XML.TyDom.Core.Generics.FromElem
    ( GFromElem
    , genericFromElem
    ) where

import Control.Applicative ((<|>))
import Data.Proxy          (Proxy (Proxy))
import Data.Text           (Text)
import GHC.Generics
import GHC.TypeLits        (KnownSymbol)

import           Text.XML.TyDom.Core.Generics.Types (OptionsElement
                                                     (optAttrName,
                                                      optConstructorElemName,
                                                      optReadLeftovers,
                                                      optReadNodeOrdering,
                                                      optSelectorElemName),
                                                     ReadLeftovers (..),
                                                     ReadNodeOrdering (..),
                                                     symt)
import           Text.XML.TyDom.Core.Types          (Attr (Attr), CData (CData),
                                                     Cause (..), Child (Child),
                                                     Content (Content),
                                                     FromElem (fromElem),
                                                     FromXText (fromXText),
                                                     Path (..),
                                                     Result (Failure, Success),
                                                     prependPath,
                                                     replacePathHead)
import           Text.XML.TyDom.Core.XMLInterface   hiding (Result (..))
import qualified Text.XML.TyDom.Core.XMLInterface   as Xi (Result (..))

-------------------------------------------------------------------------------

-- | Generic producer for a 'FromElem' instance.
genericFromElem :: (Generic z, GFromElem e n a t (Rep z), Eq n)
                => OptionsElement n a
                -> Decompose e n a t d
                -> e
                -> Result e n a t z
genericFromElem o d e = do
    let eThawed = dThaw d e
    (z, e') <- gFromElem o d eThawed
    _       <- checkLeftovers o d e'
    return (to z)

-------------------------------------------------------------------------------

-- | Class for generically converting an element to a type.
class GFromElem e n a t z where
    gFromElem :: (Eq n)
              => OptionsElement n a
              -> Decompose e n a t d
              -> d
              -> Result e n a t (z r, d)

------ Datatype and constructors

-- | D1 - Datatype (non-newtype).
instance GFromElem e n a t z =>
         GFromElem e n a t (D1 ('MetaData g h i 'False) z) where
    gFromElem o d e = mf M1 <$> gFromElem o d e

-- | Newtype.
--
--   A newtype should appear the same as the type it wraps, but the name of the
--   element is expected to be different.
--
--   To read a newtype, there are some shenanigans involved:
--     1. Check that the actual element name matches the newtype's constructor.
--     2. Rename the element to the original, wrapped type. The original type
--        must have ONLY ONE constructor, otherwise we wouldn't know what the
--        element should be renamed to.
--     3. Read in the original element and wrap it in the newtype constructor.
instance (Generic z,
          GSingleConstructorName (Rep z),
          KnownSymbol name,
          FromElem e n a t z) =>
         GFromElem e n a t
             (D1 ('MetaData g h i 'True)
                 (C1 ('MetaCons name q w)
                     (S1 s (Rec0 z)))) where
    gFromElem o d e =
        let
            origConstructor = genericSingleConstructorName (Proxy :: Proxy z)
            origElemName    = optConstructorElemName o origConstructor
            actual   = dName d e
            expected = optConstructorElemName o (symt (Proxy :: Proxy name))
        in replacePathHead actual $ do
            _ <- checkElemName expected actual
            z <- fromElem (dFreeze d (dRename d origElemName e))
            return (M1 (M1 (M1 (K1 z))), dEmpty d)

-- | U1 - no-argument constructor.
instance GFromElem e n a t U1 where
    gFromElem _ _ e = Success (U1, e)

-- | C1 - constructor.
instance (KnownSymbol name, Eq n, GFromElem e n a t z) =>
         GFromElem e n a t (C1 ('MetaCons name q w) z) where
    gFromElem o d e = prependPath (dName d e) $ do
        _ <- checkConstructorName o d (Proxy :: Proxy name) e
        (r, e') <- gFromElem o d e
        return (M1 r, e')

------  Sums and products

-- | Product type (ie. multiple fields).
instance (GFromElem e n a t z1, GFromElem e n a t z2) =>
         GFromElem e n a t (z1 :*: z2) where
    gFromElem o d e = do
        (l, e1) <- gFromElem o d e
        (r, e2) <- gFromElem o d e1
        return (l :*: r, e2)

-- | Sum type (ie. multiple constructors).
instance (GFromElem e n a t z1, GFromElem e n a t z2) =>
         GFromElem e n a t (z1 :+: z2) where
    gFromElem o d e = mf L1 <$> gFromElem o d e
                  <|> mf R1 <$> gFromElem o d e

------ Attr

-- | S1 (named) + Attr - record selector for an XML attribute.
instance (KnownSymbol name, FromXText t z) =>
         GFromElem e n a t
             (S1 ('MetaSel ('Just name) g h i) (Rec0 (Attr z))) where
    gFromElem o d e = mf (M1 . K1 . Attr) <$> getAttr o d p e
      where p = Proxy :: Proxy name

-- | S1 (named) + Attr Maybe - record selector for an optional XML attribute.
instance {-# OVERLAPS #-} (KnownSymbol name, FromXText t z) =>
         GFromElem e n a t
             (S1 ('MetaSel ('Just name) g h i) (Rec0 (Attr (Maybe z)))) where
    gFromElem o d e = mf (M1 . K1 . Attr) <$> getAttrMaybe o d p e
      where p = Proxy :: Proxy name

------ Child

-- | S1 (named) + Child - record selector for a simple child element with text
--   content.
instance (KnownSymbol name, FromXText t z) =>
         GFromElem e n a t
             (S1 ('MetaSel ('Just name) g h i) (Rec0 (Child z))) where
    gFromElem o d e =
        let p = Proxy :: Proxy name
        in do
            (child, e') <- case optReadNodeOrdering o of
                               All      -> getNextChildNamed o d p e
                               Sequence -> getChildSeqSelector o d p e
            z           <- processChild o d child
            return ((M1 . K1 . Child) z, e')

-- | S1 (named) + Child Maybe - record selector for a simple optional child
--   element with text content.
instance {-# OVERLAPS #-} (KnownSymbol name, FromXText t z) =>
         GFromElem e n a t
             (S1 ('MetaSel ('Just name) g h i) (Rec0 (Child (Maybe z)))) where
    gFromElem o d e =
        let
            p = Proxy :: Proxy name
            rChild = case optReadNodeOrdering o of
                         All      -> getNextChildNamed o d p e
                         Sequence -> getChildSeqSelector o d p e
        in case rChild of
            Failure _ _ -> Success ((M1 . K1 . Child) Nothing, e)
            Success (child, e') -> do
                z <- processChild o d child
                return ((M1 . K1 . Child . Just) z, e')

-- | S1 (named) + [Child] - record selector for a list of child elements with
--   text content.
instance {-# OVERLAPS #-} (KnownSymbol name, FromXText t z) =>
         GFromElem e n a t
             (S1 ('MetaSel ('Just name) g h i) (Rec0 (Child [z]))) where
    gFromElem o d e =
        let
            p = Proxy :: Proxy name
            (children, e') = case optReadNodeOrdering o of
                All      -> getChildListAll o d p e
                Sequence -> getChildListSeq o d p e
        in do
            zs <- sequence (processChild o d <$> children)
            return ((M1 . K1 . Child) zs, e')

------ Content

-- | S1 (named or unnamed) + Content - record selector for a content child.
instance FromXText t z =>
         GFromElem e n a t (S1 q (Rec0 (Content z))) where
    gFromElem o d e =
        let
            (t, e') = case optReadNodeOrdering o of
                          All      -> getNextContent d e
                          Sequence -> getNextSeqContent d e
        in do
            z <- parseContent t
            return ((M1 . K1 . Content) z, e')

-- | S1 (named or unnamed) + Content Maybe - record selector for an optional
--   content child.
instance {-# OVERLAPS #-} FromXText t z =>
         GFromElem e n a t (S1 q (Rec0 (Content (Maybe z)))) where
    gFromElem o d e =
        let
            tMaybe = case optReadNodeOrdering o of
                         All      -> getNextContentMaybe d e
                         Sequence -> getNextSeqContentMaybe d e
        in
            case tMaybe of
                Nothing -> Success ((M1 . K1 . Content) Nothing, e)
                Just (t, e') -> do
                    z <- parseContent t
                    return ((M1 . K1 . Content . Just) z, e')

------ CData

-- | S1 (named or unnamed) + CData - record selector for a CDATA child node.
instance FromXText t z => GFromElem e n a t (S1 q (Rec0 (CData z))) where
    gFromElem o d e = do
        (text, e') <- case optReadNodeOrdering o of
                          All      -> getNextCData d e
                          Sequence -> getNextSeqCData d e
        z <- parseCData text
        return ((M1 . K1 . CData) z, e')

-- | S1 (named or unnamed) + CData Maybe - record selector for an optional CDATA
--   child node.
instance FromXText t z =>
         GFromElem e n a t (S1 q (Rec0 (CData (Maybe z)))) where
    gFromElem o d e =
        let rText = case optReadNodeOrdering o of
                        All      -> getNextCData d e
                        Sequence -> getNextSeqCData d e
        in case rText of
            Failure _ _ -> Success ((M1 . K1 . CData) Nothing, e)
            Success (text, e') -> do
                z <- parseCData text
                return ((M1 . K1 . CData . Just) z, e')

-- | S1 (named or unnamed) + [CData] - record selector for a list of CDATA child
--   nodes.
instance FromXText t z =>
         GFromElem e n a t (S1 q (Rec0 (CData [z]))) where
    gFromElem o d e =
        let (ts, e') = case optReadNodeOrdering o of
                           All      -> getAllCData d e
                           Sequence -> getAllSeqCData d e
        in do
            zs <- sequence (parseCData <$> ts)
            return ((M1 . K1 . CData) zs, e')

------ FromElem children

-- | S1 (named or unnamed) - record selector for a 'FromElem' child.
instance {-# OVERLAPS #-} (FromElem e n a t z) =>
         GFromElem e n a t (S1 q (Rec0 z)) where
    gFromElem o d e = do
        (z, e') <- case optReadNodeOrdering o of
                       Sequence -> readNextChildSeq d e
                       All      -> readNextChild d e
        return ((M1 . K1) z, e')

-- | S1 (named or unnamed) + Maybe - record selector for an optional 'FromElem'
--   child.
instance (FromElem e n a t z) =>
         GFromElem e n a t (S1 q (Rec0 (Maybe z))) where
    gFromElem o d e = do
        (z, e') <- case optReadNodeOrdering o of
                       Sequence -> readNextChildSeqMaybe d e
                       All      -> readNextChildMaybe d e
        return ((M1 . K1) z, e')

-- | S1 (named or unnamed) + List - record selector for a list of 'FromElem'
--   children.
instance {-# OVERLAPS #-} (FromElem e n a t z) =>
         GFromElem e n a t (S1 q (Rec0 [z])) where
    gFromElem o d e =
        let (zs, e') = case optReadNodeOrdering o of
                           Sequence -> readNextChildren d e
                           All      -> readAllChildren d e
        in Success ((M1 . K1) zs, e')

-------------------------------------------------------------------------------
-- Discoverer of a single constructor name (for types that have it).

genericSingleConstructorName :: (Generic a, GSingleConstructorName (Rep a))
                             => Proxy a
                             -> Text
genericSingleConstructorName p = gSingleConstructorName (fmap from p)

class GSingleConstructorName a where
    gSingleConstructorName :: Proxy (a r) -> Text

instance (KnownSymbol name) =>
         GSingleConstructorName (D1 m (C1 ('MetaCons name q w) s)) where
    gSingleConstructorName _ = symt (Proxy :: Proxy name)

-------------------------------------------------------------------------------
-- Helper functions internal to this module.

-- | Maps a function over the first element of a tuple.
mf :: (a -> c) -> (a, b) -> (c, b)
mf f (x, y) = (f x, y)

-- | Checks for leftovers when an element has been parsed.
checkLeftovers :: OptionsElement n a
               -> Decompose e n a t d
               -> d
               -> Result e n a t ()
checkLeftovers o d e = case optReadLeftovers o of
    LeftoversOK    -> Success ()
    LeftoversError -> if dNull d e
                      then Success ()
                      else Failure PathRoot (Leftover (dFreeze d e))

-- | Checks the name of a 'Decompose' element @e@, using a constructor name, and
--   fails with a 'WrongElementName' if it is not correct.
checkConstructorName :: (KnownSymbol name, Eq n)
                     => OptionsElement n a
                     -> Decompose e n a t d
                     -> Proxy name
                     -> d
                     -> Result e n a t ()
checkConstructorName o d p e =
    checkElemName (optConstructorElemName o (symt p)) (dName d e)

-- | If two element names are not equal, fail with a 'WrongElementName'.
checkElemName :: (Eq n) => n -> n -> Result e n a t ()
checkElemName expected actual =
    if expected == actual
    then Success ()
    else Failure PathRoot (WrongElementName expected actual)

-- | Gets attribute text, fails with 'MissingAttribute'.
getAttrT :: Decompose e n a t d -> a -> d -> Result e n a t (t, d)
getAttrT d attrName e = case dAttr d attrName e of
    Xi.Success e' t -> Success (t, e')
    Xi.Failure      -> Failure PathRoot (MissingAttribute attrName)

-- | Parses attribute text, fails with 'FailParseAttribute'
parseAttrT :: FromXText t z => a -> (t, d) -> Result e n a t (z, d)
parseAttrT attrName (text, e) = case fromXText text of
    Right z  -> Success (z, e)
    Left err -> Failure PathRoot (FailParseAttribute attrName err)

-- | Gets an attribute.
getAttr :: (KnownSymbol name, FromXText t z)
        => OptionsElement n a
        -> Decompose e n a t d
        -> Proxy name
        -> d
        -> Result e n a t (z, d)
getAttr o d p e =
    let attrName = optAttrName o (symt p)
    in getAttrT d attrName e >>= parseAttrT attrName

-- | Gets an optional attribute.
getAttrMaybe :: (KnownSymbol name, FromXText t z)
             => OptionsElement n a
             -> Decompose e n a t d
             -> Proxy name
             -> d
             -> Result e n a t (Maybe z, d)
getAttrMaybe o d p e =
    let attrName = optAttrName o (symt p)
    in case getAttrT d attrName e of
        Failure _ _        -> Success (Nothing, e)
        Success (text, d') -> mf Just <$> parseAttrT attrName (text, d')

-- | Fetches a child in sequence, whose name must match a value obtained using
--   'optSelectorElemName'. Fails with 'MissingElement' or 'WrongElementName'.
getChildSeqSelector :: (KnownSymbol name, Eq n)
                    => OptionsElement n a
                    -> Decompose e n a t d
                    -> Proxy name
                    -> d
                    -> Result e n a t (d, d)
getChildSeqSelector o d p e =
    let expected = optSelectorElemName o (symt p)
    in case dNextSeqChild d e of
        Xi.Failure -> Failure PathRoot (MissingElement expected)
        Xi.Success e' child ->
            let actual = dName d child
            in if actual == expected
               then Success (child, e')
               else Failure PathRoot (WrongElementName expected actual)

-- | Fetches the next child with a given name. Fails with 'MissingElement'.
getNextChildNamed :: (KnownSymbol name)
                  => OptionsElement n a
                  -> Decompose e n a t d
                  -> Proxy name
                  -> d
                  -> Result e n a t (d, d)
getNextChildNamed o d p e =
    let expected = optSelectorElemName o (symt p)
    in case dNextChildNamed d expected e of
        Xi.Failure          -> Failure PathRoot (MissingElement expected)
        Xi.Success e' child -> Success (child, e')

-- | When a child element's name matches the one provided, return it as a Just,
--   and converted to a 'Decompose' element.
adoptChild :: Eq n => Decompose e n a t d -> n -> e -> Maybe d
adoptChild d childName child =
    let
        el = dThaw d child
    in if dName d el == childName
       then Just el
       else Nothing

-- | Fetches all sequential children whose name matches the given proxy,
--   after mapping through 'optSelectorElemName'.
getChildListSeq :: (KnownSymbol name, Eq n)
                => OptionsElement n a
                -> Decompose e n a t d
                -> Proxy name
                -> d
                -> ([d], d)
getChildListSeq o d p e =
    let childName = optSelectorElemName o (symt p)
    in case dSuccessNextChildren d e (adoptChild d childName) of
        (e', cs) -> (cs, e')

-- | Fetches all children whose name matches the given proxy, after mapping
--   through 'optSelectorElemName'.
getChildListAll :: (KnownSymbol name, Eq n)
                => OptionsElement n a
                -> Decompose e n a t d
                -> Proxy name
                -> d
                -> ([d], d)
getChildListAll o d p e =
    let childName = optSelectorElemName o (symt p)
    in case dSuccessChildren d e (adoptChild d childName) of
        (e', cs) -> (cs, e')

-- | Fetches all content from an element. Fails with 'MissingContent'.
getAllContent :: Decompose e n a t d -> d -> Result e n a t (t, d)
getAllContent d child = case dAllContent d child of
    Xi.Failure      -> Failure PathRoot MissingContent
    Xi.Success e' t -> Success (t, e')

-- | Parses content. Fails with 'FailParseContent'.
parseContent :: FromXText t z => t -> Result e n a t z
parseContent text = case fromXText text of
    Left err -> Failure PathRoot (FailParseContent err)
    Right z  -> Success z

-- | Processes a child node that has text, returning the converted value.
processChild :: FromXText t z
             => OptionsElement n a
             -> Decompose e n a t d
             -> d
             -> Result e n a t z
processChild o d child =
    let
        childName = dName d child
    in
        prependPath childName $ do
        (text, child') <- getAllContent d child
        _              <- checkLeftovers o d child'
        z              <- parseContent text
        return z

-- | Fetches next sequential content node, or a blank string if there is none.
getNextSeqContent :: Decompose e n a t d -> d -> (t, d)
getNextSeqContent d e = case getNextSeqContentMaybe d e of
    Just x  -> x
    Nothing -> (dEmptyTxt d, e)

-- | Fetches next content node, or a blank string if there is none.
getNextContent :: Decompose e n a t d -> d -> (t, d)
getNextContent d e = case getNextContentMaybe d e of
    Just x  -> x
    Nothing -> (dEmptyTxt d, e)

-- | Fetches next sequential content node as a Maybe result.
getNextSeqContentMaybe :: Decompose e n a t d -> d -> Maybe (t, d)
getNextSeqContentMaybe d e = case dNextSeqContent d e of
    Xi.Failure      -> Nothing
    Xi.Success e' t -> Just (t, e')

-- | Fetches next content node as a Maybe result
getNextContentMaybe :: Decompose e n a t d -> d -> Maybe (t, d)
getNextContentMaybe d e = case dNextContent d e of
    Xi.Failure      -> Nothing
    Xi.Success e' t -> Just (t, e')

-- | Parses CDATA node, fails with 'FailParseCData'.
parseCData :: FromXText t z => t -> Result e n a t z
parseCData text = case fromXText text of
    Left err -> Failure PathRoot (FailParseCData err)
    Right z  -> Success z

-- | Fetches next sequential CDATA node, fails with 'MissingCData'.
getNextSeqCData :: Decompose e n a t d -> d -> Result e n a t (t, d)
getNextSeqCData d e = case dNextSeqCData d e of
    Xi.Failure      -> Failure PathRoot MissingCData
    Xi.Success e' t -> Success (t, e')

-- | Fetches next CDATA node, fails with 'MissingCData'.
getNextCData :: Decompose e n a t d -> d -> Result e n a t (t, d)
getNextCData d e = case dNextCData d e of
    Xi.Failure      -> Failure PathRoot MissingCData
    Xi.Success e' t -> Success (t, e')

-- | Fetches CDATA nodes sequentially; should not fail (just empty list).
getAllSeqCData :: Decompose e n a t d -> d -> ([t], d)
getAllSeqCData d e = case dAllNextCData d e of
    (e', ts) -> (ts, e')

-- | Fetches all CDATA nodes; should not fail (just empty list).
getAllCData :: Decompose e n a t d -> d -> ([t], d)
getAllCData d e = case dAllCData d e of
    (e', ts) -> (ts, e')

-- | Fetches the next child element in sequence. Fails with 'NoMoreElements'.
getNextChildSeq :: Decompose e n a t d -> d -> Result e n a t (d, d)
getNextChildSeq d e = case dNextSeqChild d e of
    Xi.Failure          -> Failure PathRoot NoMoreElements
    Xi.Success e' child -> Success (child, e')

-- | Reads the next child element in sequence.
readNextChildSeq :: FromElem e n a t z
                 => Decompose e n a t d
                 -> d
                 -> Result e n a t (z, d)
readNextChildSeq d e = do
    (child, e') <- getNextChildSeq d e
    z <- fromElem (dFreeze d child)
    return (z, e')

-- | Reads the next child element (not necessarily in sequence).
readNextChild :: forall e n a t d z
               . FromElem e n a t z
              => Decompose e n a t d
              -> d
              -> Result e n a t (z, d)
readNextChild d e =
    let f child = case fromElem child :: Result e n a t z of
                      Failure _ _ -> Nothing
                      Success z   -> Just z
    in case dSuccessChild d e f of
        Xi.Failure      -> Failure PathRoot NoMoreElements
        Xi.Success e' z -> Success (z, e')

-- | Optionally reads the next child element in sequence.
readNextChildSeqMaybe :: FromElem e n a t z
                      => Decompose e n a t d
                      -> d
                      -> Result e n a t (Maybe z, d)
readNextChildSeqMaybe d e = case getNextChildSeq d e of
    Failure _ _ -> Success (Nothing, e)
    Success (child, e') -> do
        z <- fromElem (dFreeze d child)
        return (Just z, e')

-- | Optionally reads the next child element (not necessarily in sequence).
readNextChildMaybe :: forall e n a t d z
                    . FromElem e n a t z
                   => Decompose e n a t d
                   -> d
                   -> Result e n a t (Maybe z, d)
readNextChildMaybe d e =
    let f child = case fromElem child :: Result e n a t z of
                      Failure _ _ -> Nothing
                      Success z   -> Just z
    in case dSuccessChild d e f of
        Xi.Failure      -> Success (Nothing, e)
        Xi.Success e' z -> Success (Just z, e')

-- | Reads all children that succeed a 'FromElem' parse.
readNextChildren :: forall e n a t d z
                  . FromElem e n a t z
                 => Decompose e n a t d
                 -> d
                 -> ([z], d)
readNextChildren d e =
    let f child = case fromElem child :: Result e n a t z of
                      Failure _ _ -> Nothing
                      Success z   -> Just z
    in case dSuccessNextChildren d e f of
        (e', cs) -> (cs, e')

-- | Reads all children sequentially that succeed a 'FromElem' parse.
readAllChildren :: forall e n a t d z
                 . FromElem e n a t z
                => Decompose e n a t d
                -> d
                -> ([z], d)
readAllChildren d e =
    let f child = case fromElem child :: Result e n a t z of
                      Failure _ _ -> Nothing
                      Success z   -> Just z
    in case dSuccessChildren d e f of
        (e', cs) -> (cs, e')
