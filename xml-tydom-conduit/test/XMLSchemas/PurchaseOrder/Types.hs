{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module XMLSchemas.PurchaseOrder.Types where

import qualified Data.Char          as C (toLower)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)
import           Text.Printf        (printf)
import           Text.Read          (readMaybe)

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck     hiding (Success)

import qualified Text.XML               as XML
import           Text.XML.TyDom.Conduit

-------------------------------------------------------------------------------

-- XML schema from: https://www.w3.org/TR/xmlschema-0/
-- Section 2. Basic Concepts: The Purchase Order

-- In this example, we assume that it's OK to litter the types that will be
-- used for the app data model with XML stuff (Attr, Child, etc.). Please see
-- the InternationalPurchaseOrder example for a case where the app data model
-- has been separated from the XML encoding.

data PurchaseOrder
    = PurchaseOrder
    { poOrderDate :: Attr Day
    , poShipTo    :: ShipTo
    , poBillTo    :: BillTo
    , poComment   :: Child (Maybe Text)
    , poItems     :: Items
    } deriving (Eq, Show, Generic)

data Items = Items [Item] deriving (Eq, Show, Generic)

data USAddress
    = USAddress
    { usCountry :: Attr Text
    , usName    :: Child Text
    , usStreet  :: Child Text
    , usCity    :: Child Text
    , usState   :: Child Text
    , usZip     :: Child Int
    } deriving (Eq, Show, Generic)

newtype ShipTo = ShipTo { unShipTo :: USAddress }
               deriving (Eq, Show, Generic, Arbitrary)

newtype BillTo = BillTo { unBillTo :: USAddress }
               deriving (Eq, Show, Generic, Arbitrary)

data Item
    = Item
    { itPartNum     :: Attr PartNum
    , itProductName :: Child Text
    , itQuantity    :: Child Int
    , itUSPrice     :: Child Price
    , itComment     :: Child (Maybe Text)
    , itShipDate    :: Child (Maybe Day)
    } deriving (Eq, Show, Generic)

data Price
    = Price
    { prDollars :: Integer
    , prCents   :: Integer
    } deriving (Eq, Show, Generic)

data PartNum
    = PartNum
    { pnNum   :: Int
    , pnAlpha :: Text
    } deriving (Eq, Show, Generic)

-------------------------------------------------------------------------------

instance ToXText Price where
    toXText (Price d c) = T.concat [ (T.pack . show) d, ".", (T.pack . show) c ]
instance FromXText Price where
    fromXText text =
        let
            (dtxt, ctxt) = T.break ((==) '.') text
            maybePrice = do
                dollars <- readMaybe (T.unpack dtxt)
                cents   <- readMaybe (T.unpack $ T.drop 1 ctxt)
                return $ Price dollars cents
        in case maybePrice of
            Just price -> Right price
            Nothing    -> xTextErrType "Price" text

instance ToXText PartNum where
    toXText (PartNum n a) = T.concat [ (T.pack . printf "%03d") n, "-", a ]
instance FromXText PartNum where
    fromXText text =
        let
            maybePartNum = if T.length text /= 6
                           then Nothing
                           else
                               let
                                   n = (readMaybe . T.unpack) $ T.take 3 text
                                   a = Just $ T.drop 4 text
                               in
                                   PartNum <$> n <*> a
        in case maybePartNum of
            Just partNum -> Right partNum
            Nothing      -> xTextErrType "PartNum" text

-------------------------------------------------------------------------------

opt :: OptionsElement
opt = defaultOptionsElement
    { optConstructorElemName = constructorElemName
    , optSelectorElemName    = selectorElemName
    , optAttrName            = attrName }

lowerFirstChar :: Text -> Text
lowerFirstChar t = T.cons (C.toLower $ T.head t) (T.tail t)

plainName :: Text -> XML.Name
plainName t = XML.Name t Nothing Nothing

constructorElemName :: Text -> ElemName
constructorElemName = ElemName . plainName . lowerFirstChar

selectorElemName :: Text -> ElemName
selectorElemName "itUSPrice"
    = ElemName $ plainName "USPrice" -- because: yay for inconsistency!!!
selectorElemName t = ElemName . plainName . lowerFirstChar . (T.drop 2) $ t

attrName :: Text -> AttrName
attrName = AttrName . plainName . lowerFirstChar . (T.drop 2)

instance ToElem PurchaseOrder where toElem = genericToElem opt
instance ToElem Items         where toElem = genericToElem opt
instance ToElem USAddress     where toElem = genericToElem opt
instance ToElem ShipTo        where toElem = genericToElem opt
instance ToElem BillTo        where toElem = genericToElem opt
instance ToElem Item          where toElem = genericToElem opt
instance FromElem PurchaseOrder where fromElem = genericFromElem opt
instance FromElem Items         where fromElem = genericFromElem opt
instance FromElem USAddress     where fromElem = genericFromElem opt
instance FromElem ShipTo        where fromElem = genericFromElem opt
instance FromElem BillTo        where fromElem = genericFromElem opt
instance FromElem Item          where fromElem = genericFromElem opt

instance Arbitrary PurchaseOrder where
    arbitrary = PurchaseOrder <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance Arbitrary Items where
    arbitrary = Items <$> arbitrary

instance Arbitrary USAddress where
    arbitrary = USAddress <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance Arbitrary Item where
    arbitrary = Item <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary Price where
    arbitrary = Price <$> arbitrary <*> arbitrary

instance Arbitrary PartNum where
    arbitrary = do
        a1 <- arbitrary
        a2 <- arbitrary
        n  <- choose (0, 999)
        let alpha = T.pack [ a1, a2 ]
        return $ PartNum n alpha
