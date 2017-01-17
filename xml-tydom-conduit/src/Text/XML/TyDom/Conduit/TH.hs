{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.XML.TyDom.Conduit.TH
    ( makeEncoding
    ) where

import Text.XML.TyDom.Core.Types (Conv, conv)
import Text.XML.TyDom.Conduit (ToElem, FromElem, toElem, fromElem, Result)
import Text.XML.TyDom.Core.Generics (genericConv)
import qualified Text.XML as XML (Element)

import Language.Haskell.TH

makeEncoding :: Name -> Name -> Q [Dec]
makeEncoding name encodingName = runQ
    [d|
        instance Conv $(conT name) $(conT encodingName) where
            conv = genericConv
        instance Conv $(conT encodingName) $(conT name) where
            conv = genericConv
        instance ToElem $(conT name) where
            toElem =
                let
                    toEncoding :: $(conT name) -> $(conT encodingName)
                    toEncoding = conv
                in
                    toElem . toEncoding
        instance FromElem $(conT name) where
            fromElem =
                let
                    fromEncoding :: $(conT encodingName) -> $(conT name)
                    fromEncoding = conv

                    fromElemEncoding :: XML.Element
                                     -> Result $(conT encodingName)
                    fromElemEncoding = fromElem
                in
                    fmap fromEncoding . fromElemEncoding
    |]
