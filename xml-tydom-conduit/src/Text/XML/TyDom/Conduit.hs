-- {-# OPTIONS_GHC -Wwarn #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Text.XML.TyDom.Conduit
    ( -- * Guide

      -- ** Quick start
      -- $quickstart

      -- ** XML text
      -- $toxmltext

      -- ** Available encodings
      -- $availEncodings

      -- ** Newtype aliasing
      -- $newtypeAliasing

      -- ** Sum types
      -- $sumTypes

      -- ** Encoding options
      -- $encodingOptions

      -- ** Separating encodings
      -- $separatingEncoding

      -- ** Error handling
      -- $errorHandling

      -- ** Reading non-sequenced XML
      -- $readingNonSequenced

      -- * Classes
      ToElem (toElem)
    , FromElem (fromElem)
    , ToXText (toXText)
    , FromXText (fromXText)
    , Conv (conv)
      -- * Types
    , Attr (Attr)
    , Child (Child)
    , Content (Content)
    , AttrName (AttrName)
    , ElemName (ElemName)
    , XTextError (XTextError)
    , Result (Success, Failure)
    , Path (PathItem, PathRoot)
    , Cause (Cause, Leftover, WrongElementName, MissingAttribute,
             FailParseAttribute, FailParseContent, FailParseCData,
             MissingElement, NoMoreElements, MissingContent,
             MissingCData, NoSuccessfulChildren)
    , OptionsElement(OptionsElement)
    , ReadNodeOrdering(All, Sequence)
    , ReadLeftovers(LeftoversError, LeftoversOK)
      -- * Generics
    , genericToElem
    , genericFromElem
    , genericConv
      -- * Functions
    , unAttr
    , unChild
    , unContent
    , unAttrName
    , unElemName
    , expectedElementName
    , actualElementName
    , optConstructorElemName
    , optSelectorElemName
    , optAttrName
    , optReadNodeOrdering
    , optReadLeftovers
    , defaultOptionsElement
    , render
    , renderFailure
    , parse
    , ignoreWSContent
    , getAttrValue
    , xTextErrType
    ) where

import GHC.Generics (Generic, Rep)

import qualified Data.Char          as C (isSpace)
import           Data.List          (break, partition)
import qualified Data.Map           as Map (delete, empty, insert, lookup, null)
import           Data.Maybe         (catMaybes, fromMaybe, mapMaybe)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as Text (all, concat, empty, null, pack,
                                             unpack, unlines, lines, intercalate)
import qualified Data.Text.Lazy     as Text (fromStrict, toStrict)
import qualified Data.Time.Calendar as Time (Day, showGregorian)
import           Text.Read          (readMaybe)

import qualified Text.XML as XML (Document (..), Element (..), Name (..),
                                  Node (..), Prologue (..), RenderSettings, def,
                                  documentRoot, elementAttributes, elementName,
                                  elementNodes, parseText, renderText)

import           Text.XML.TyDom.Core.Generics     (ReadNodeOrdering (..),
                                                   ReadLeftovers (..),
                                                   genericConv)
import qualified Text.XML.TyDom.Core.Generics     as G (GFromElem, GToElem,
                                                        OptionsElement (..),
                                                        genericFromElem,
                                                        genericToElem,
                                                        optAttrName,
                                                        optConstructorElemName,
                                                        optReadNodeOrdering,
                                                        optReadLeftovers,
                                                        optSelectorElemName)
import           Text.XML.TyDom.Core.Types        (Attr (..), Child (..),
                                                   Content (..), Conv, conv,
                                                   unAttr, unChild, unContent,
                                                   XTextError (..),
                                                   xTextErrType)
import qualified Text.XML.TyDom.Core.Types        as T (Cause (..), FromElem,
                                                        FromXText, Path (..),
                                                        Result (..), ToElem,
                                                        ToXText, fromElem,
                                                        fromXText, toElem,
                                                        toXText)
import           Text.XML.TyDom.Core.XMLInterface (Compose (..), Decompose (..),
                                                   cAttr, cCData, cChild,
                                                   cContent, cEmpty, cFreeze,
                                                   cName, cNull, cThaw,
                                                   dAllCData, dAllContent,
                                                   dAllNextCData, dAttr, dEmpty,
                                                   dEmptyTxt, dFreeze, dName,
                                                   dNextCData, dNextChildNamed,
                                                   dNextContent, dNextSeqCData,
                                                   dNextSeqChild,
                                                   dNextSeqContent, dNull,
                                                   dRename, dSuccessChild,
                                                   dSuccessChildren,
                                                   dSuccessNextChildren, dThaw)
import qualified Text.XML.TyDom.Core.XMLInterface as XMLi (Result (..))


-------------------------------------------------------------------------------

newtype AttrName = AttrName { unAttrName :: XML.Name } deriving (Eq, Show)
newtype ElemName = ElemName { unElemName :: XML.Name } deriving (Eq, Show)

class ToElem a where
    toElem :: a -> XML.Element

class FromElem a where
    fromElem :: XML.Element -> Result a

class ToXText a where
    toXText :: a -> Text

class FromXText a where
    fromXText :: Text -> Either XTextError a

instance ToXText a => T.ToXText Text a where
    toXText = toXText

instance FromXText a => T.FromXText Text a where
    fromXText = fromXText

instance ToElem a => T.ToElem XML.Element a where
    toElem = toElem

instance FromElem a => T.FromElem XML.Element ElemName AttrName Text a where
    fromElem = toTyResult . fromElem

data Result a = Success a
              | Failure Path Cause
              deriving (Eq, Show, Functor)

fromTyResult :: T.Result XML.Element ElemName AttrName Text a -> Result a
fromTyResult (T.Success x)   = Success x
fromTyResult (T.Failure p c) = Failure (fromTyPath p) (fromTyCause c)

toTyResult :: Result a -> T.Result XML.Element ElemName AttrName Text a
toTyResult (Success x)   = T.Success x
toTyResult (Failure p c) = T.Failure (toTyPath p) (toTyCause c)

instance Applicative Result where
    pure = Success
    Success f   <*> Success a   = Success (f a)
    _           <*> Failure p c = Failure p c
    Failure p c <*> _           = Failure p c

instance Monad Result where
    Success x   >>= f = f x
    Failure p c >>= _ = Failure p c

data Path = PathItem ElemName Path
          | PathRoot
          deriving (Eq, Show)

fromTyPath :: T.Path ElemName -> Path
fromTyPath (T.PathItem n p) = PathItem n (fromTyPath p)
fromTyPath T.PathRoot       = PathRoot

toTyPath :: Path -> T.Path ElemName
toTyPath (PathItem n p) = T.PathItem n (toTyPath p)
toTyPath PathRoot       = T.PathRoot

data Cause
    = Cause Text
    | Leftover XML.Element
    | WrongElementName
    { expectedElementName :: ElemName
    , actualElementName   :: ElemName }
    | MissingAttribute AttrName
    | FailParseAttribute AttrName XTextError
    | FailParseContent XTextError
    | FailParseCData XTextError
    | MissingElement ElemName
    | NoMoreElements
    | MissingContent
    | MissingCData
    | NoSuccessfulChildren
    deriving (Eq, Show)

fromTyCause :: T.Cause XML.Element ElemName AttrName Text -> Cause
fromTyCause (T.Cause t)                = Cause t
fromTyCause (T.Leftover e)             = Leftover e
fromTyCause (T.WrongElementName e a)   = WrongElementName e a
fromTyCause (T.MissingAttribute n)     = MissingAttribute n
fromTyCause (T.FailParseAttribute n e) = FailParseAttribute n e
fromTyCause (T.FailParseContent e)     = FailParseContent e
fromTyCause (T.FailParseCData e)       = FailParseCData e
fromTyCause (T.MissingElement n)       = MissingElement n
fromTyCause T.NoMoreElements           = NoMoreElements
fromTyCause T.MissingContent           = MissingContent
fromTyCause T.MissingCData             = MissingCData
fromTyCause T.NoSuccessfulChildren     = NoSuccessfulChildren

toTyCause :: Cause -> T.Cause XML.Element ElemName AttrName Text
toTyCause (Cause t)                = T.Cause t
toTyCause (Leftover e)             = T.Leftover e
toTyCause (WrongElementName e a)   = T.WrongElementName e a
toTyCause (MissingAttribute n)     = T.MissingAttribute n
toTyCause (FailParseAttribute n e) = T.FailParseAttribute n e
toTyCause (FailParseContent e)     = T.FailParseContent e
toTyCause (FailParseCData e)       = T.FailParseCData e
toTyCause (MissingElement n)       = T.MissingElement n
toTyCause NoMoreElements           = T.NoMoreElements
toTyCause MissingContent           = T.MissingContent
toTyCause MissingCData             = T.MissingCData
toTyCause NoSuccessfulChildren     = T.NoSuccessfulChildren

-------------------------------------------------------------------------------

instance ToXText Text where toXText = id
instance FromXText Text where fromXText = Right

instance ToXText Int where toXText = Text.pack . show
instance FromXText Int where
    fromXText t = case (readMaybe . Text.unpack) t of
        Just i  -> Right i
        Nothing -> xTextErrType "Int" t

instance ToXText Time.Day where toXText = Text.pack . Time.showGregorian
instance FromXText Time.Day where
    fromXText t = case (readMaybe . Text.unpack) t of
        Just i  -> Right i
        Nothing -> xTextErrType "Day" t

-------------------------------------------------------------------------------

render :: XML.Element -> Text
render e =
    let
        rs = XML.def :: XML.RenderSettings
        prologue = XML.Prologue [] Nothing []
        document = XML.Document prologue e []
    in
        Text.toStrict $ XML.renderText rs document

parse :: Text -> Result XML.Element
parse t = case XML.parseText XML.def (Text.fromStrict t) of
    Left ex   -> Failure PathRoot (Cause ((Text.pack . show) ex))
    Right doc -> Success (XML.documentRoot doc)

ignoreWSContent :: XML.Element -> XML.Element
ignoreWSContent e =
    let
        f (XML.NodeElement c)     = Just $ XML.NodeElement (ignoreWSContent c)
        f c@(XML.NodeContent txt) = if Text.all C.isSpace txt
                                    then Nothing
                                    else Just c
        f n                       = Just n
        nodes' = mapMaybe f (XML.elementNodes e)
    in
        e { XML.elementNodes = nodes' }

-------------------------------------------------------------------------------

getAttrValue :: (FromXText a) => AttrName -> XML.Element -> Result a
getAttrValue an@(AttrName n) e = case Map.lookup n (XML.elementAttributes e) of
    Nothing -> Failure (PathItem (ElemName (XML.elementName e)) PathRoot)
                       (MissingAttribute an)
    Just text -> case fromXText text of
        Left err -> Failure (PathItem (ElemName (XML.elementName e)) PathRoot)
                            (FailParseAttribute an err)
        Right a  -> Success a

-------------------------------------------------------------------------------

data OptionsElement = OptionsElement
    { optConstructorElemName :: Text -> ElemName
    , optSelectorElemName    :: Text -> ElemName
    , optAttrName            :: Text -> AttrName
    , optReadNodeOrdering    :: ReadNodeOrdering
    , optReadLeftovers       :: ReadLeftovers }

defaultOptionsElement :: OptionsElement
defaultOptionsElement = OptionsElement
    { optConstructorElemName = \t -> ElemName (XML.Name t Nothing Nothing)
    , optSelectorElemName    = \t -> ElemName (XML.Name t Nothing Nothing)
    , optAttrName            = \t -> AttrName (XML.Name t Nothing Nothing)
    , optReadNodeOrdering    = Sequence
    , optReadLeftovers       = LeftoversError }

optionsElementTy :: OptionsElement -> G.OptionsElement ElemName AttrName
optionsElementTy o = G.OptionsElement
    { G.optConstructorElemName = optConstructorElemName o
    , G.optSelectorElemName    = optSelectorElemName o
    , G.optAttrName            = optAttrName o
    , G.optReadNodeOrdering    = optReadNodeOrdering o
    , G.optReadLeftovers       = optReadLeftovers o }

-- | Generic producer for 'FromElem' instances.
genericFromElem :: (Generic z,
                    G.GFromElem XML.Element ElemName AttrName Text (Rep z))
                => OptionsElement
                -> XML.Element
                -> Result z
genericFromElem o e = fromTyResult
                      $ G.genericFromElem
                        (optionsElementTy o) decomposeConduit e

-- | Generic producer for 'ToElem' instances.
genericToElem :: (Generic z,
                  G.GToElem XML.Element ElemName AttrName Text (Rep z))
              => OptionsElement
              -> z
              -> XML.Element
genericToElem o z = G.genericToElem (optionsElementTy o) composeConduit z

-------------------------------------------------------------------------------

composeConduit :: Compose XML.Element ElemName AttrName Text XML.Element
composeConduit = Compose
    { cEmpty   = ccEmpty
    , cName    = ccName
    , cAttr    = ccAttr
    , cChild   = ccChild
    , cContent = ccContent
    , cCData   = ccCData
    , cFreeze  = ccFreeze
    , cThaw    = ccThaw
    , cNull    = ccNull
    }

decomposeConduit :: Decompose XML.Element ElemName AttrName Text XML.Element
decomposeConduit = Decompose
    { dThaw                = cdThaw
    , dFreeze              = cdFreeze
    , dEmpty               = cdEmpty
    , dNull                = cdNull
    , dName                = cdName
    , dRename              = cdRename
    , dAttr                = cdAttr
    , dNextSeqChild        = cdNextSeqChild
    , dNextSeqContent      = cdNextSeqContent
    , dNextSeqCData        = cdNextSeqCData
    , dNextChildNamed      = cdNextChildNamed
    , dNextContent         = cdNextContent
    , dNextCData           = cdNextCData
    , dSuccessChild        = cdSuccessChild
    , dSuccessNextChildren = cdSuccessNextChildren
    , dSuccessChildren     = cdSuccessChildren
    , dAllContent          = cdAllContent
    , dAllNextCData        = cdAllNextCData
    , dAllCData            = cdAllCData
    , dEmptyTxt            = cdEmptyTxt
    }

-------------------------------------------------------------------------------

ccEmpty :: XML.Element
ccEmpty = XML.Element (XML.Name "" Nothing Nothing) Map.empty []

ccName :: ElemName -> XML.Element -> XML.Element
ccName name e = e { XML.elementName = unElemName name }

ccAttr :: AttrName -> Text -> XML.Element -> XML.Element
ccAttr name value e =
    let
        attr  = XML.elementAttributes e
        attr' = Map.insert (unAttrName name) value attr
    in
        e { XML.elementAttributes = attr' }

ccChild :: XML.Element -> XML.Element -> XML.Element
ccChild child parent =
    let
        nodes' = XML.NodeElement child : XML.elementNodes parent
    in
        parent { XML.elementNodes = nodes' }

ccContent :: Text -> XML.Element -> XML.Element
ccContent text e =
    let
        nodes' = XML.NodeContent text : XML.elementNodes e
    in
        e { XML.elementNodes = nodes' }

ccCData :: Text -> XML.Element -> XML.Element
ccCData = error "CDATA not yet implemented for xml-conduit"

ccFreeze :: XML.Element -> XML.Element
ccFreeze e =
    let
        nodes' = reverse (XML.elementNodes e)
    in
        e { XML.elementNodes = nodes' }

ccThaw :: XML.Element -> XML.Element
ccThaw e =
    let
        nodes' = reverse (XML.elementNodes e)
    in
        e { XML.elementNodes = nodes' }

ccNull :: Text -> Bool
ccNull = Text.null

-------------------------------------------------------------------------------

cdThaw :: XML.Element -> XML.Element
cdThaw = id

cdFreeze :: XML.Element -> XML.Element
cdFreeze = id

cdEmpty :: XML.Element
cdEmpty = XML.Element (XML.Name "" Nothing Nothing) Map.empty []

cdNull :: XML.Element -> Bool
cdNull e = null (XML.elementNodes e) && Map.null (XML.elementAttributes e)

cdName :: XML.Element -> ElemName
cdName e = ElemName (XML.elementName e)

cdRename :: ElemName -> XML.Element -> XML.Element
cdRename (ElemName n) e = e { XML.elementName = n }

cdAttr :: AttrName -> XML.Element -> XMLi.Result XML.Element Text
cdAttr name e =
    let
        txtName = unAttrName name
        attrs   = XML.elementAttributes e
        attr    = Map.lookup txtName attrs
        attrs'  = Map.delete txtName attrs
        e'      = e { XML.elementAttributes = attrs' }
    in
        fromMaybe XMLi.Failure (XMLi.Success e' <$> attr)

cdNextSeqChild :: XML.Element -> XMLi.Result XML.Element XML.Element
cdNextSeqChild e = case XML.elementNodes e of
    XML.NodeElement child : ns' ->
        let
            e' = e { XML.elementNodes = ns' }
        in
            XMLi.Success e' child
    _ -> XMLi.Failure

cdNextSeqContent :: XML.Element -> XMLi.Result XML.Element Text
cdNextSeqContent e = case XML.elementNodes e of
    XML.NodeContent child : ns' ->
        let
            e' = e { XML.elementNodes = ns' }
        in
            XMLi.Success e' child
    _ -> XMLi.Failure

cdNextSeqCData :: XML.Element -> XMLi.Result XML.Element Text
cdNextSeqCData = error "dNextSeqCData not supported for xml-conduit"

cdNextChildNamed :: ElemName
                 -> XML.Element
                 -> XMLi.Result XML.Element XML.Element
cdNextChildNamed (ElemName name) e =
    let
        f (XML.NodeElement child) | XML.elementName child == name = True
        f _                       = False
        (before, at) = break f (XML.elementNodes e)
    in case at of
        XML.NodeElement child : ns' ->
            let
                e' = e { XML.elementNodes = before <> ns' }
            in
                XMLi.Success e' child
        _ -> XMLi.Failure

cdNextContent :: XML.Element -> XMLi.Result XML.Element Text
cdNextContent e =
    let
        f (XML.NodeContent _) = True
        f _                   = False
        (before, at) = break f (XML.elementNodes e)
    in case at of
        XML.NodeContent text : ns' ->
            let
                e' = e { XML.elementNodes = before <> ns' }
            in
                XMLi.Success e' text
        _ -> XMLi.Failure

cdNextCData :: XML.Element -> XMLi.Result XML.Element Text
cdNextCData = error "dNextCData not yet implemented for xml-conduit"

cdSuccessChild :: XML.Element
               -> (XML.Element -> Maybe a)
               -> XMLi.Result XML.Element a
cdSuccessChild e f =
    let
        g node@(XML.NodeElement child) = (node, f child)
        g node                         = (node, Nothing)
        h (_, Just _) = True
        h _           = False
        (before, at) = break h $ g <$> XML.elementNodes e
    in case at of
        (_, Just r) : ns' ->
            let
                e' = e { XML.elementNodes = fst <$> (before <> ns') }
            in
                XMLi.Success e' r
        _ -> XMLi.Failure

cdSuccessNextChildren :: XML.Element
                      -> (XML.Element -> Maybe a)
                      -> (XML.Element, [a])
cdSuccessNextChildren e f =
    let
        g node@(XML.NodeElement child) = (node, f child)
        g node                         = (node, Nothing)
        h (_, Just _) = True
        h _           = False
        (at, after) = span h $ g <$> XML.elementNodes e
        as = catMaybes $ snd <$> at
        e' = e { XML.elementNodes = fst <$> after }
    in
        (e', as)

cdSuccessChildren :: XML.Element
                  -> (XML.Element -> Maybe a)
                  -> (XML.Element, [a])
cdSuccessChildren e f =
    let
        g node@(XML.NodeElement child) = (node, f child)
        g node                         = (node, Nothing)
        h (_, Just _) = True
        h _           = False
        (succeeded, others) = partition h $ g <$> XML.elementNodes e
        as = catMaybes $ snd <$> succeeded
        e' = e { XML.elementNodes = fst <$> others }
    in
        (e', as)

cdAllContent :: XML.Element -> XMLi.Result XML.Element Text
cdAllContent e =
    let
        g (XML.NodeContent _) = True
        g _                   = False
        (contents, others) = partition g $ XML.elementNodes e
        textFn (XML.NodeContent t) = Just t
        textFn _                   = Nothing
        text = Text.concat $ mapMaybe textFn contents
        e' = e { XML.elementNodes = others }
    in
        XMLi.Success e' text

cdAllNextCData :: XML.Element -> (XML.Element, [Text])
cdAllNextCData = error "dAllNextCData not yet implemented for xml-conduit."

cdAllCData :: XML.Element -> (XML.Element, [Text])
cdAllCData = error "dAllCData not yet implemented for xml-conduit."

cdEmptyTxt :: Text
cdEmptyTxt = Text.empty

-------------------------------------------------------------------------------

-- Error rendering

pathToList :: Path -> [ElemName]
pathToList = reverse . pathToList'
  where
    pathToList' PathRoot              = []
    pathToList' (PathItem name path') = name : pathToList' path'

renderPath :: Path -> Text
renderPath path =
    let
        names = (XML.nameLocalName . unElemName) <$> (pathToList path)
    in
        if null names
        then "(No Path)"
        else Text.concat [ "Path: ", Text.intercalate " -> " names ]

causeToText :: Cause -> Text
causeToText (Cause t) = t
causeToText (Leftover el) = Text.unlines
    [ "The following element components were left-over after reading:"
    , Text.unlines $ drop 1 $ Text.lines (render el) ]
causeToText (WrongElementName expected actual) = Text.concat
    [ "Expected element ["
    , XML.nameLocalName $ unElemName expected
    , "] but encountered ["
    , XML.nameLocalName $ unElemName actual
    , "]"]
causeToText (MissingAttribute attrName) = Text.concat
    [ "Missing attribute [", XML.nameLocalName $ unAttrName attrName, "]" ]
causeToText (FailParseAttribute attrName xTextError) = Text.concat
    [ "Bad attribute ["
    , XML.nameLocalName $ unAttrName attrName
    , "]: "
    , unXTextError xTextError ]
causeToText (FailParseContent xTextError) = Text.concat
    [ "Bad content node: ", unXTextError xTextError ]
causeToText (FailParseCData xTextError) = Text.concat
    [ "Bad CDATA node: ", unXTextError xTextError ]
causeToText (MissingElement elemName) = Text.concat
    [ "Could not locate element ["
    , XML.nameLocalName $ unElemName elemName
    , "]" ]
causeToText NoMoreElements = "Expected more elements"
causeToText MissingContent = "Expected a content node"
causeToText MissingCData   = "Expected a CDATA node"
causeToText NoSuccessfulChildren =
    "Did not read any child elements successfully"

renderFailure :: Result a -> Text
renderFailure (Success _) = "Well, this is embarrassing - not a failure!"
renderFailure (Failure path cause) =
    Text.unlines [ renderPath path, causeToText cause ]

-------------------------------------------------------------------------------

-- $quickstart
--
-- @xml-tydom@ is a library for expressing XML representations using Haskell
-- data types. The serialization to and from XML is done automatically using GHC
-- Generics and (optionally) some Template Haskell. A good way to illustrate
-- this is with a quick example.
--
-- We start with a Haskell data type that describes the XML structure we want:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics (Generic)
--
-- data Person = Person
--     { id      :: 'Attr' Int      -- an attribute
--     , name    :: 'Child' Text    -- a child element containing text
--     , comment :: 'Content' Text  -- a child text content node
--     } deriving (Show, Generic)
-- @
--
-- Then we use GHC Generics to write instances of 'ToElem' and 'FromElem' for
-- the @Person@ type (you can probably guess what these do):
--
-- @
-- instance 'ToElem' Person where
--     'toElem' = 'genericToElem' 'defaultOptionsElement'
-- instance 'FromElem' Person where
--     'fromElem' = 'genericFromElem' 'defaultOptionsElement'
-- @
--
-- With these typeclass instances available, we can serialize a value of type
-- @Person@ to @Text@ containing XML, and also read back the generated @Text@:
--
-- @
-- >>> person = Person ('Attr' 42) ('Child' \"Joe\") ('Content' \"XML4Joe!\")
--
-- >>> text = 'render' $ 'toElem' person
-- >>> text
-- \"\<Person id=\\\"42\\\">\<name>Joe\<\/name>XML4Joe!\<\/Person>\"
--
-- >>> personResult = ('parse' text >>= 'fromElem') :: 'Result' Person
-- 'Success'
--     (Person
--         { id      = 'Attr'    { 'unAttr'    = 42         }
--         , name    = 'Child'   { 'unChild'   = \"Joe\"      }
--         , comment = 'Content' { 'unContent' = \"XML4Joe!\" }
--         })
-- @

-- $toxmltext
--
-- Textual content in XML documents can appear as either attributes or text
-- content nodes within elements. The conversion of types to and from text is
-- controlled by a pair of typeclasses:
--
--     ['ToXText'@ a@] Converts type @a@ to @Text@.
--
--     ['FromXText'@ a@] Converts @Text@ to @Either 'XTextError' a@.
--
-- It is recommended that instances of these typeclasses should be written
-- manually for most user-defined types.

-- $availEncodings
--
-- The following types exist to represent parts of the XML DOM:
-- 
--     [@{ selectorName = @'Attr'@ a }@] A value of type @a@ will become an
--     attribute of the element, containing the textual representation of @a@.
--     The name of the attribute is specified by the @selectorName@, which
--     /must/ be supplied for the field.
--
--     [@{ selectorName = @'Child'@ a }@] A value of type @a@ will become a
--     child element. The name of the child element is specified by the
--     @selectorName@, which /must/ be supplied for the field. The child element
--     will contain a text node containing the textual representation of @a@.
--
--     [@{ selectorName = @'Content'@ a }@] A value of type @a@ will become a
--     text node of the element. The @selectorName@ is not used in the encoding
--     to XML, and is optional.
--
--     [@{ selectorName = @'a'@ }@] Value @a@ will become a child element. The
--     @selectorName@ is optional and is not used in the encoding to XML. There
--     must be an appropriate instance of 'ToElem' and / or 'FromElem' for the
--     type @a@.
--
-- In addition to these wrappers in their basic form, they can also be combined
-- with @Maybe@ and lists to create optional and list DOM parts. The following
-- combinations are supported automatically:
--
--     * 'Attr'@ a@
--     * 'Attr'@ (Maybe a)@
--     * 'Child'@ a@
--     * 'Child'@ (Maybe a)@
--     * 'Child'@ [a]@
--     * 'Content'@ a@
--     * 'Content'@ (Maybe a)@
--
-- The case of 'Attr'@ [a]@ is not supported because there is no obvious
-- encoding for more than one value of an attribute. Similarly, 'Content'@ [a]@
-- would be problematic because a list of text content nodes could not be
-- separated from each other trivially. To encode lists in attributes or text
-- content, instances of 'ToXText'@ [a]@ / 'FromXText'@ [a]@ can be supplied
-- for type @a@ that can handle case-specific encoding.

-- $newtypeAliasing
--
-- In addition to the basic encoding types ('Attr', 'Child' and 'Content'), it
-- is possible to alias an entire element using a @newtype@. An instance for a
-- @newtype@ created using 'genericToElem' / 'genericFromElem' will use the
-- encoding for the wrapped type with the name of the @newtype@ constructor.
--
-- For example:
--
-- @
-- data    Port   = Port   { Content Int      } deriving (Show, Generic)
-- newtype InPort = InPort { unInPort :: Port } deriving (Show, Generic)
--
-- opt = 'defaultOptionsElement'
-- instance 'ToElem' Port   where 'toElem' = 'genericToElem' opt
-- instance 'ToElem' InPort where 'toElem' = 'genericToElem' opt
--
-- >>> render $ toElem (Port (Content 443))
-- \"\<Port>443\<\/Port>\" 
-- >>> render $ toElem (InPort (Port (Content 443)))
-- \"\<InPort>443\<\/InPort>\"
-- @

-- $sumTypes
--
-- The name of an element is always specified by the name of the constructor in
-- Haskell. Sum types, with multiple constructors, are also supported in a
-- straightforward way. These can represent cases where one element can be
-- chosen from a selection of elements (ie. @\<xsd:choice>@ in an XML schema).
--
-- For example:
--
-- @
-- data Ref = Id   { id   :: 'Attr'    Int  }
--          | Name { name :: 'Content' Text }
--          deriving (Show, Generic)
--
-- opt = 'defaultOptionsElement'
-- instance 'ToElem' Ref where 'toElem' = 'genericToElem' opt
-- instance 'FromElem' Ref where 'fromElem' = 'genericFromElem' opt
--
-- >>> text = 'render' $ 'toElem' (Name ('Content' "Martok"))
-- >>> text
-- \"\<Name>Martok\<\/Name>\"
--
-- >>> refResult = ('parse' text >>= 'fromElem') :: 'Result' Ref
-- 'Success' (Name { name = 'Content' { 'unContent' = \"Martok\" } })
-- @

-- $encodingOptions
--
-- Several options exist for the encoding. These are specified by
-- 'OptionsElement', which is passed as an argument to 'genericToElem' and
-- 'genericFromElem'. The following can be specified:
--
--     * Bijections from constructor and selector names to element names and
--       attribute names.
--     * Whether nodes should be sequential (ie. @\<xsd:sequence\/>@) or can
--       appear in any order when reading. Nodes are /always/ written
--       sequentially.
--     * Whether an error is produced when extra attributes or nodes exist in
--       the XML but not in the Haskell datatype.
--
-- The naming bijections are particularly useful because often a particular XML
-- schema will require names that are not directly representable as Haskell
-- constructors or selectors. For example, XML names may start with lowercase
-- characters, or they may require hyphens, or namespaces. In these instances, a
-- function can be provided which must perform a bijective (one-to-one) mapping
-- between the textual representation of an element or attribute name and its
-- required XML name. As a simple example, we may want to drop the first two
-- characters of a selector name:
--
-- @
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Text.XML as XML  -- xml-conduit
--
-- opt = 'defaultOptionsElement' { 'optAttrName' = attrName }
--
-- attrName = Text -> 'AttrName'
-- attrName selectorName
--     = 'AttrName' $ XML.Name (T.drop 2 selectorName) Nothing Nothing
--
-- data Address = Address { adName :: 'Attr' Text } deriving (Show, Generic)
--                   --->   ^^ - drop these two letters from the attribute name
-- instance 'ToElem' Address where 'toElem' = 'genericToElem' opt
--
-- >>> 'render' $ 'toElem' (Address ('Attr' "Josephine Citizen"))
-- \"\<Address Name=\\\"Josephine Citizen\\\"\/>\"
-- @
--
-- Both 'AttrName' and 'ElemName' are @newtype@ wrappers around @XML.Name@.

-- $separatingEncoding
--
-- Specifying the encoding in Haskell is clumsy, because types are littered with
-- mentions of 'Attr', 'Child' and 'Content'. From a practical perspective,
-- these are quite ugly if they appear in the application's data model. They
-- conflate the concerns of data representation and serialization, which should
-- be separate.
--
-- We can improve this situation by using one type for the application's
-- own data model and a separate type for the encoding. @xml-tydom@ provides
-- some Template Haskell support to ease this process. For example:
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Text.XML.TyDom.Conduit.TH ('makeEncoding')
--
-- -- Data type for the application (plain; no 'Attr', 'Child' or 'Content')
-- data Address = Address
--     { name   :: Text
--     , street :: Text
--     , city   :: Text
--     , zip    :: Int
--     } deriving (Show, Generic)
--
-- -- Data type specifying the encoding. This must have the same form as the
-- -- application data type, except for mentions of 'Attr', 'Child' and
-- -- 'Content'.
-- data EncAddress = EncAddress
--     { encName   :: 'Child' Text
--     , encStreet :: 'Child' Text
--     , encCity   :: 'Child' Text
--     , encZip    :: 'Child' Int
--     } deriving (Show, Generic)
--
-- -- We need to specify /both/ 'ToElem' and 'FromElem' instances for the
-- -- encoding type (the Template Haskell operation requires both):
-- instance 'ToElem' EncAddress where
--     'toElem' = 'toElem' 'defaultOptionsElement'
-- instance 'FromElem' EncAddress where
--     'fromElem' = 'fromElem' 'defaultOptionsElement'
--
-- -- But having done this, we can get Template Haskell to write instances for
-- -- the application type (Address). Instances are supplied for:
-- --   - 'ToElem' Address
-- --   - 'FromElem' Address
-- --   - 'Conv' Address AddressEnc
-- --   - 'Conv' AddressEnc Address
-- \$('makeEncoding' \'\'Address \'\'EncAddress)
-- @
--
-- If you use this approach, the names of attributes and elements are specified
-- using the /encoding type/ (@EncAddress@ in the above example), and __not__
-- the application data type. Under the hood, to produce XML, the application
-- data type is first converted to the encoding type (using a Generic
-- converter), and then the encoding type is converted to XML. The reverse
-- process is followed to read from XML. Because the encoding (and thus the
-- 'OptionsElement') is specified completely by the encoding type, the required
-- 'ToElem' and 'FromElem' instances for the application type are completely
-- unambiguous.

-- $errorHandling
--
-- Reading from XML to a type can fail. The result of reading from XML is the
-- 'Result' type, which is a disjunction specifying either 'Success' or
-- 'Failure'. In the event of a 'Failure', the 'Path' to the failed element from
-- the document root is recorded, as is a detailed 'Cause' of the failure. If
-- you want a convenient textual representation of the failure, this can be
-- achieved with the 'renderFailure' function. For example:
--
-- @
-- import qualified Data.Text.IO as T (putStr)
--
-- path  = 'PathItem' ('ElemName' (XML.Name "Root" Nothing Nothing)) 'PathRoot'
-- cause = 'MissingAttribute' ('AttrName' (XML.Name "myAttr" Nothing Nothing))
-- >>> T.putStr $ 'renderFailure' ('Failure' path cause)
-- Path: Root
-- Missing attribute [myAttr]
-- @

-- $readingNonSequenced
--
-- Often, we are faced with reading child elements whose order is not
-- guaranteed. @xml-tydom@ supports this to the greatest extent that is
-- feasible. To enable non-sequential reading, 'optReadChildOrdering' must be
-- set to 'All' in the 'OptionsElement' that is used to generate the 'FromElem'
-- instance. The handling of different cases can be addressed separately:
--
--     [content] The first text content is accepted.
--
--     [optional content] If no content is present then this becomes 'Nothing'.
--
--     [child element] The first child element which succeeds in 'fromElem'
--     is accepted.
--
--     [optional child element] If no child element succeeds in 'fromElem' then
--     this becomes 'Nothing'.
--
--     [list of child elements] Every child element which succeeds in
--     'fromElem' becomes part of the list.
--
-- Given these rules, it should become apparent that certain combinations are
-- __not__ valid for elements that are read as 'All'. For example, while a data
-- type like the following is OK for 'Sequence' elements, it will fail for 'All'
-- elements:
--
-- @
-- -- This will work for a 'Sequence' read, but not an 'All' read
-- data OnlyOkForSequenced = OnlyOkForSequenced
--     { aWidgets :: [Widgets]
--     , grommit  :: Grommit
--     , bWidgets :: [Widgets]
--     } deriving (Show, Generic)
-- @
--
-- However, similar rules also exist for 'Sequence' reads, although they are
-- somewhat more obvious:
--
-- @
-- -- This will __FAIL__ to read! __DO NOT ACTUALLY USE IT__
-- -- We can't possibly tell where the list of widget child elements ends, so
-- -- all of them will be consumed, leaving no remaining widget for the final
-- -- member of the datatype.
-- data Bad = Bad
--     { aWidgets :: [Widgets]
--     , widget   :: Widget
--     } deriving (Show, Generic)
-- @
