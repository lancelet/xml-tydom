{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}

module XMLSchemas.PurchaseOrder.Spec (tests) where

import XMLSchemas.PurchaseOrder.Types

import Data.Time.Calendar (fromGregorian)

import RoundTrips.Basics     (roundTrip)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Success)

import qualified Text.XML               as XML
import           Text.XML.TyDom.Conduit

-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "XMLSchemas.PurchaseOrder"
    [ testLoad
    , testRoundTrip ]

-------------------------------------------------------------------------------

testData :: FilePath
testData = "test/data/purchase-order.xml"

-------------------------------------------------------------------------------

testLoad :: TestTree
testLoad = testCase "Loading purchase order example" $ do
    (XML.Document _ element _) <- XML.readFile XML.def testData
    let
        poResult = fromElem (ignoreWSContent element) :: Result PurchaseOrder
        expected =
            Success $
            PurchaseOrder
            (Attr (fromGregorian 1999 10 20))
            (ShipTo $ USAddress
                (Attr "US")
                (Child "Alice Smith")
                (Child "123 Maple Street")
                (Child "Mill Valley")
                (Child "CA")
                (Child 90952))
            (BillTo $ USAddress
                (Attr "US")
                (Child "Robert Smith")
                (Child "8 Oak Avenue")
                (Child "Old Town")
                (Child "PA")
                (Child 95819))
            (Child $ Just "Hurry, my lawn is going wild!")
            (Items [ Item
                       (Attr (PartNum 872 "AA"))
                       (Child "Lawnmower")
                       (Child 1)
                       (Child (Price 148 95))
                       (Child $ Just "Confirm this is electric")
                       (Child Nothing)
                   , Item
                       (Attr (PartNum 926 "AA"))
                       (Child "Baby Monitor")
                       (Child 1)
                       (Child (Price 39 98))
                       (Child Nothing)
                       (Child $ Just (fromGregorian 1999 05 21))])
    expected @=? poResult

testRoundTrip :: TestTree
testRoundTrip = testProperty "PurchaseOrder roundtrip"
                (roundTrip :: PurchaseOrder -> Bool)
