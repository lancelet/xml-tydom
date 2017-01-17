{-# OPTIONS_GHC -Wwarn #-}

module Main (main) where

import Test.Tasty

import qualified RoundTrips.Basics             as Basics (tests)
import qualified XMLSchemas.PurchaseOrder.Spec as XMLSchemasPurchaseOrder (tests)

import Text.XML.TyDom.Conduit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Basics.tests
    , XMLSchemasPurchaseOrder.tests ]
