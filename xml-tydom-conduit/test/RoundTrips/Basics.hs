{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module RoundTrips.Basics (tests, roundTrip) where

import           Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Test.QuickCheck           hiding (Result, Success)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck     hiding (Success)

import qualified Text.XML as XML (Element)

import Text.XML.TyDom.Conduit

tests :: TestTree
tests = testGroup "Basics"
    [ testSingletonToElem
    , testSingletonFromElem
      --
    , testAttrToElem
    , testAttrFromElem
    , testAttrRoundTrip
    , testAttrMaybeToElem
    , testAttrMaybeFromElem
    , testAttrMaybeRoundTrip
      --
    , testChildToElem
    , testChildFromElem
    , testChildRoundTrip
    , testChildMaybeToElem
    , testChildMaybeFromElem
    , testChildMaybeRoundTrip
    , testChildListToElem
    , testChildListFromElem
    , testChildListRoundTrip
      --
    , testContentToElem
    , testContentFromElem
    , testContentRoundTrip
    , testContentMaybeToElem
    , testContentMaybeFromElem
    , testContentMaybeRoundTrip
      --
    , testElementToElem
    , testElementFromElem
    , testElementRoundTrip
    , testElementMaybeToElem
    , testElementMaybeFromElem
    , testElementMaybeRoundTrip
    , testElementListToElem
    , testElementListFromElem
    , testElementListRoundTrip
      --
    , testMultiConstructorsToElem
    , testMultiConstructorsFromElem
    , testMultiConstructorsRoundTrip
      --
    , testNewtypeToElem
    , testNewtypeFromElem
    , testNewtypeRoundTrip
    ]

xmlHeader :: Text
xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

-------------------------------------------------------------------------------

opt :: OptionsElement
opt = defaultOptionsElement { optReadLeftovers = LeftoversError }

optAll :: OptionsElement
optAll = defaultOptionsElement
         { optReadLeftovers    = LeftoversError
         , optReadNodeOrdering = All }

toElemTest :: ToElem a => a -> Text -> Assertion
toElemTest a expected = expected @=? (render (toElem a))

fromElemTest :: (Eq a, Show a, FromElem a) => Text -> a -> Assertion
fromElemTest text expected = (Success expected) @=? (parse text >>= fromElem)

fromElemAll :: (Eq a, Show a)
            => (XML.Element -> Result a) -> Text -> a -> Assertion
fromElemAll from text expected = (Success expected) @=? (parse text >>= from)

roundTrip :: (Eq a, ToElem a, FromElem a) => a -> Bool
roundTrip a = (parse (render $ toElem a) >>= fromElem) == Success a

-------------------------------------------------------------------------------

-- Raw singleton element

data TSingleton = TSingleton deriving (Eq, Show, Generic)
instance ToElem TSingleton where
    toElem = genericToElem opt
instance FromElem TSingleton where
    fromElem = genericFromElem opt

testSingletonToElem :: TestTree
testSingletonToElem = testCase "TSingleton toElem" $
    toElemTest
    TSingleton
    (T.concat [ xmlHeader, "<TSingleton/>" ])

testSingletonFromElem :: TestTree
testSingletonFromElem = testCase "TSingleton fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader, "<TSingleton/>" ])
        TSingleton)
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TSingleton/>" ])
        TSingleton)

-------------------------------------------------------------------------------

-- Element with an attribute

data TAttr
    = TAttr
    { attr :: Attr Text
    } deriving (Eq, Show, Generic)
instance ToElem TAttr where
    toElem = genericToElem opt
instance FromElem TAttr where
    fromElem = genericFromElem opt
instance Arbitrary TAttr where
    arbitrary = TAttr <$> arbitrary

testAttrToElem :: TestTree
testAttrToElem = testCase "TAttr toElem" $
    toElemTest
    (TAttr (Attr "attrTxt"))
    (T.concat [ xmlHeader, "<TAttr attr=\"attrTxt\"/>"])

testAttrFromElem :: TestTree
testAttrFromElem = testCase "TAttr fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader, "<TAttr attr=\"attrTxt\"/>" ])
        (TAttr (Attr "attrTxt")))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TAttr attr=\"attrTxt\"/>" ])
        (TAttr (Attr "attrTxt")))

testAttrRoundTrip :: TestTree
testAttrRoundTrip = testProperty "TAttr roundtrip" (roundTrip :: TAttr -> Bool)

-------------------------------------------------------------------------------

-- Element with an optional attribute

data TAttrMaybe
    = TAttrMaybe
    { attrMaybe :: Attr (Maybe Text)
    } deriving (Eq, Show, Generic)
instance ToElem TAttrMaybe where
    toElem = genericToElem opt
instance FromElem TAttrMaybe where
    fromElem = genericFromElem opt
instance Arbitrary TAttrMaybe where
    arbitrary = TAttrMaybe <$> arbitrary

testAttrMaybeToElem :: TestTree
testAttrMaybeToElem = testCase "TAttrMaybe toElem" $ do
    (toElemTest
        (TAttrMaybe (Attr (Just "attrTxt")))
        (T.concat [ xmlHeader, "<TAttrMaybe attrMaybe=\"attrTxt\"/>" ]))
    (toElemTest
        (TAttrMaybe (Attr Nothing))
        (T.concat [ xmlHeader, "<TAttrMaybe/>" ]))

testAttrMaybeFromElem :: TestTree
testAttrMaybeFromElem = testCase "TAttrMaybe fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader, "<TAttrMaybe attrMaybe=\"attrTxt\"/>" ])
        (TAttrMaybe (Attr (Just "attrTxt"))))
    (fromElemTest
        (T.concat [ xmlHeader, "<TAttrMaybe/>" ])
        (TAttrMaybe (Attr Nothing)))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TAttrMaybe attrMaybe=\"attrTxt\"/>" ])
        (TAttrMaybe (Attr (Just "attrTxt"))))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TAttrMaybe/>" ])
        (TAttrMaybe (Attr Nothing)))

testAttrMaybeRoundTrip :: TestTree
testAttrMaybeRoundTrip
    = testProperty "TAttrMaybe roundtrip" (roundTrip :: TAttrMaybe -> Bool)

-------------------------------------------------------------------------------

-- Element with a child containing text

data TChild
    = TChild
    { child :: Child Text
    } deriving (Eq, Show, Generic)
instance ToElem TChild where
    toElem = genericToElem opt
instance FromElem TChild where
    fromElem = genericFromElem opt
instance Arbitrary TChild where
    arbitrary = TChild <$> arbitrary

testChildToElem :: TestTree
testChildToElem = testCase "TChild toElem" $
    toElemTest
    (TChild (Child "child text"))
    (T.concat [ xmlHeader, "<TChild><child>child text</child></TChild>" ])

testChildFromElem :: TestTree
testChildFromElem = testCase "TChild fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader, "<TChild><child>child text</child></TChild>" ])
        (TChild (Child "child text")))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TChild><child>child text</child></TChild>" ])
        (TChild (Child "child text")))

testChildRoundTrip :: TestTree
testChildRoundTrip
    = testProperty "TChild roundtrip" (roundTrip :: TChild -> Bool)

-------------------------------------------------------------------------------

-- Element with an optional child containing text

data TChildMaybe
    = TChildMaybe
    { childMaybe :: Child (Maybe Text)
    } deriving (Eq, Show, Generic)
instance ToElem TChildMaybe where
    toElem = genericToElem opt
instance FromElem TChildMaybe where
    fromElem = genericFromElem opt
instance Arbitrary TChildMaybe where
    arbitrary = TChildMaybe <$> arbitrary

testChildMaybeToElem :: TestTree
testChildMaybeToElem = testCase "TChildMaybe toElem" $ do
    (toElemTest
        (TChildMaybe (Child (Just "maybe child")))
        (T.concat [ xmlHeader
                  , "<TChildMaybe>"
                  , "<childMaybe>maybe child</childMaybe>"
                  , "</TChildMaybe>" ]))
    (toElemTest
        (TChildMaybe (Child Nothing))
        (T.concat [ xmlHeader, "<TChildMaybe/>"]))

testChildMaybeFromElem :: TestTree
testChildMaybeFromElem = testCase "TChildMaybe fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader
                  , "<TChildMaybe>"
                  , "<childMaybe>maybe child</childMaybe>"
                  , "</TChildMaybe>" ])
        (TChildMaybe (Child (Just "maybe child"))))
    (fromElemTest
        (T.concat [ xmlHeader, "<TChildMaybe/>" ])
        (TChildMaybe (Child Nothing)))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader
                  , "<TChildMaybe>"
                  , "<childMaybe>maybe child</childMaybe>"
                  , "</TChildMaybe>" ])
        (TChildMaybe (Child (Just "maybe child"))))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TChildMaybe/>" ])
        (TChildMaybe (Child Nothing)))

testChildMaybeRoundTrip :: TestTree
testChildMaybeRoundTrip
    = testProperty "TChildMaybe roundtrip" (roundTrip :: TChildMaybe -> Bool)

-------------------------------------------------------------------------------

-- Element with a list of children

data TChildList
    = TChildList
    { childList :: Child [Text]
    } deriving (Eq, Show, Generic)
instance ToElem TChildList where
    toElem = genericToElem opt
instance FromElem TChildList where
    fromElem = genericFromElem opt
instance Arbitrary TChildList where
    arbitrary = TChildList <$> arbitrary

testChildListToElem :: TestTree
testChildListToElem = testCase "TChildList toElem" $
    toElemTest
    (TChildList (Child ["a item", "b item"]))
    (T.concat [ xmlHeader
              , "<TChildList>"
              , "<childList>a item</childList>"
              , "<childList>b item</childList>"
              , "</TChildList>"])

testChildListFromElem :: TestTree
testChildListFromElem = testCase "TChildList fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader
                  , "<TChildList>"
                  , "<childList>a item</childList>"
                  , "<childList>b item</childList>"
                  , "</TChildList>" ])
        (TChildList (Child ["a item", "b item"])))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader
                  , "<TChildList>"
                  , "<childList>a item</childList>"
                  , "<childList>b item</childList>"
                  , "</TChildList>" ])
        (TChildList (Child ["a item", "b item"])))

testChildListRoundTrip :: TestTree
testChildListRoundTrip
    = testProperty "TChildList roundtrip" (roundTrip :: TChildList -> Bool)

-------------------------------------------------------------------------------

-- Element with content

data TContent
    = TContent
    { content :: Content Text
    } deriving (Eq, Show, Generic)
instance ToElem TContent where
    toElem = genericToElem opt
instance FromElem TContent where
    fromElem = genericFromElem opt
instance Arbitrary TContent where
    arbitrary = TContent <$> arbitrary

testContentToElem :: TestTree
testContentToElem = testCase "TContent toElem" $ do
    (toElemTest
        (TContent (Content "some content"))
        (T.concat [ xmlHeader, "<TContent>some content</TContent>" ]))
    (toElemTest
        (TContent (Content ""))
        (T.concat [ xmlHeader, "<TContent/>" ]))

testContentFromElem :: TestTree
testContentFromElem = testCase "TContent fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader, "<TContent>some content</TContent>" ])
        (TContent (Content "some content")))
    (fromElemTest
        (T.concat [ xmlHeader, "<TContent/>" ])
        (TContent (Content "")))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TContent>some content</TContent>" ])
        (TContent (Content "some content")))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TContent/>" ])
        (TContent (Content "")))

testContentRoundTrip :: TestTree
testContentRoundTrip
    = testProperty "TContent roundtrip" (roundTrip :: TContent -> Bool)

-------------------------------------------------------------------------------

-- Element with optional content

data TContentMaybe
    = TContentMaybe
    { contentMaybe :: Content (Maybe Text)
    } deriving (Eq, Show, Generic)
instance ToElem TContentMaybe where
    toElem = genericToElem opt
instance FromElem TContentMaybe where
    fromElem = genericFromElem opt
instance Arbitrary TContentMaybe where
    arbitrary = TContentMaybe <$> arbitrary

testContentMaybeToElem :: TestTree
testContentMaybeToElem = testCase "TContentMaybe toElem" $ do
    (toElemTest
        (TContentMaybe (Content (Just "content")))
        (T.concat [ xmlHeader
                  , "<TContentMaybe>"
                  , "content"
                  , "</TContentMaybe>" ]))
    (toElemTest
        (TContentMaybe (Content Nothing))
        (T.concat [ xmlHeader, "<TContentMaybe/>" ]))
    (toElemTest
        (TContentMaybe (Content (Just "")))
        (T.concat [ xmlHeader, "<TContentMaybe/>" ]))

testContentMaybeFromElem :: TestTree
testContentMaybeFromElem = testCase "TContentMaybe fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader
                  , "<TContentMaybe>"
                  , "content"
                  , "</TContentMaybe>" ])
        (TContentMaybe (Content (Just "content"))))
    (fromElemTest
        (T.concat [ xmlHeader, "<TContentMaybe/>" ])
        (TContentMaybe (Content Nothing)))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader
                  , "<TContentMaybe>"
                  , "content"
                  , "</TContentMaybe>" ])
        (TContentMaybe (Content (Just "content"))))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TContentMaybe/>" ])
        (TContentMaybe (Content Nothing)))

newtype NonEmptyText = NonEmptyText { unNonEmptyText :: Text }
    deriving (Eq, Show, ToXText, FromXText)
instance Arbitrary NonEmptyText where
    arbitrary = do
        genText <- arbitrary
        return $ NonEmptyText (T.concat ["_", genText])
data TContentMaybeNonEmpty
    = TContentMaybeNonEmpty
    { contentMaybeNonEmpty :: Content (Maybe NonEmptyText)
    } deriving (Eq, Show, Generic)
instance ToElem TContentMaybeNonEmpty where
    toElem = genericToElem opt
instance FromElem TContentMaybeNonEmpty where
    fromElem = genericFromElem opt
instance Arbitrary TContentMaybeNonEmpty where
    arbitrary = TContentMaybeNonEmpty <$> arbitrary

testContentMaybeRoundTrip :: TestTree
testContentMaybeRoundTrip = testProperty "TContentMaybeNonEmpty roundtrip"
                            (roundTrip :: TContentMaybeNonEmpty -> Bool )

-------------------------------------------------------------------------------

-- Element with element content

data TElement
    = TElement
    { element :: TAttr
    } deriving (Eq, Show, Generic)
instance ToElem TElement where
    toElem = genericToElem opt
instance FromElem TElement where
    fromElem = genericFromElem opt
instance Arbitrary TElement where
    arbitrary = TElement <$> arbitrary

testElementToElem :: TestTree
testElementToElem = testCase "TElement toElem" $
    toElemTest
    (TElement (TAttr (Attr "value")))
    (T.concat [ xmlHeader
              , "<TElement>"
              , "<TAttr attr=\"value\"/>"
              , "</TElement>" ])

testElementFromElem :: TestTree
testElementFromElem = testCase "TElement fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader
                  , "<TElement>"
                  , "<TAttr attr=\"value\"/>"
                  , "</TElement>" ])
        (TElement (TAttr (Attr "value"))))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader
                  , "<TElement>"
                  , "<TAttr attr=\"value\"/>"
                  , "</TElement>" ])
        (TElement (TAttr (Attr "value"))))

testElementRoundTrip :: TestTree
testElementRoundTrip
    = testProperty "TElement roundtrip" (roundTrip :: TElement -> Bool)

-------------------------------------------------------------------------------

-- Element with optional element content

data TElementMaybe
    = TElementMaybe
    { elementMaybe :: Maybe TAttr
    } deriving (Eq, Show, Generic)
instance ToElem TElementMaybe where
    toElem = genericToElem opt
instance FromElem TElementMaybe where
    fromElem = genericFromElem opt
instance Arbitrary TElementMaybe where
    arbitrary = TElementMaybe <$> arbitrary

testElementMaybeToElem :: TestTree
testElementMaybeToElem = testCase "TElementMaybe toElem" $ do
    (toElemTest
        (TElementMaybe (Just (TAttr (Attr "value"))))
        (T.concat [ xmlHeader
                  , "<TElementMaybe>"
                  , "<TAttr attr=\"value\"/>"
                  , "</TElementMaybe>" ]))
    (toElemTest
        (TElementMaybe Nothing)
        (T.concat [ xmlHeader, "<TElementMaybe/>" ]))

testElementMaybeFromElem :: TestTree
testElementMaybeFromElem = testCase "TElementMaybe fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader
                  , "<TElementMaybe>"
                  , "<TAttr attr=\"value\"/>"
                  , "</TElementMaybe>" ])
        (TElementMaybe (Just (TAttr (Attr "value")))))
    (fromElemTest
        (T.concat [ xmlHeader, "<TElementMaybe/>" ])
        (TElementMaybe Nothing))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader
                  , "<TElementMaybe>"
                  , "<TAttr attr=\"value\"/>"
                  , "</TElementMaybe>" ])
        (TElementMaybe (Just (TAttr (Attr "value")))))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TElementMaybe/>" ])
        (TElementMaybe Nothing))

testElementMaybeRoundTrip :: TestTree
testElementMaybeRoundTrip
    = testProperty "TElementMaybe roundtrip"
      (roundTrip :: TElementMaybe -> Bool)

-------------------------------------------------------------------------------

-- Element with element list content

data TElementList
    = TElementList
    { elementList :: [TAttr]
    } deriving (Eq, Show, Generic)
instance ToElem TElementList where
    toElem = genericToElem opt
instance FromElem TElementList where
    fromElem = genericFromElem opt
instance Arbitrary TElementList where
    arbitrary = TElementList <$> arbitrary

testElementListToElem :: TestTree
testElementListToElem = testCase "TElementList toElem" $
    toElemTest
    (TElementList [(TAttr (Attr "attr 1")), (TAttr (Attr "attr 2"))])
    (T.concat [ xmlHeader
              , "<TElementList>"
              , "<TAttr attr=\"attr 1\"/>"
              , "<TAttr attr=\"attr 2\"/>"
              , "</TElementList>" ])

testElementListFromElem :: TestTree
testElementListFromElem = testCase "TElementList fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader
                  , "<TElementList>"
                  , "<TAttr attr=\"attr 1\"/>"
                  , "<TAttr attr=\"attr 2\"/>"
                  , "</TElementList>" ])
        (TElementList [(TAttr (Attr "attr 1")), (TAttr (Attr "attr 2"))]))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader
                  , "<TElementList>"
                  , "<TAttr attr=\"attr 1\"/>"
                  , "<TAttr attr=\"attr 2\"/>"
                  , "</TElementList>" ])
        (TElementList [(TAttr (Attr "attr 1")), (TAttr (Attr "attr 2"))]))

testElementListRoundTrip :: TestTree
testElementListRoundTrip
    = testProperty "TestElementList roundtrip"
      (roundTrip :: TElementList -> Bool)

-------------------------------------------------------------------------------

-- Multiple constructors

data MultiConstructor
    = ElemA { aAttr :: Attr Text }
    | ElemB { bAttr :: Attr Text }
    deriving (Eq, Show, Generic)
instance ToElem MultiConstructor where
    toElem = genericToElem opt
instance FromElem MultiConstructor where
    fromElem = genericFromElem opt
instance Arbitrary MultiConstructor where
    arbitrary = oneof
        [ ElemA <$> arbitrary
        , ElemB <$> arbitrary ]

testMultiConstructorsToElem :: TestTree
testMultiConstructorsToElem = testCase "MultiConstructor toElem" $ do
    (toElemTest
        (ElemA (Attr "a value"))
        (T.concat [ xmlHeader, "<ElemA aAttr=\"a value\"/>" ]))
    (toElemTest
        (ElemB (Attr "b value"))
        (T.concat [ xmlHeader, "<ElemB bAttr=\"b value\"/>" ]))

testMultiConstructorsFromElem :: TestTree
testMultiConstructorsFromElem = testCase "MultiConstructor fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader, "<ElemA aAttr=\"a value\"/>" ])
        (ElemA (Attr "a value")))
    (fromElemTest
        (T.concat [ xmlHeader, "<ElemB bAttr=\"b value\"/>" ])
        (ElemB (Attr "b value")))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<ElemA aAttr=\"a value\"/>" ])
        (ElemA (Attr "a value")))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<ElemB bAttr=\"b value\"/>" ])
        (ElemB (Attr "b value")))

testMultiConstructorsRoundTrip :: TestTree
testMultiConstructorsRoundTrip = testProperty "MultiConstructor roundtrip"
                                 (roundTrip :: MultiConstructor -> Bool)

-------------------------------------------------------------------------------

-- Newtype (alias another element)

newtype TNewtype = TNewtype { unTNewType :: TAttr } deriving (Eq, Show, Generic)
instance ToElem TNewtype where
    toElem = genericToElem opt
instance FromElem TNewtype where
    fromElem = genericFromElem opt
instance Arbitrary TNewtype where
    arbitrary = TNewtype <$> arbitrary

testNewtypeToElem :: TestTree
testNewtypeToElem = testCase "TNewtype toElem" $
    toElemTest
    (TNewtype (TAttr (Attr "attr txt")))
    (T.concat [ xmlHeader, "<TNewtype attr=\"attr txt\"/>" ])

testNewtypeFromElem :: TestTree
testNewtypeFromElem = testCase "TNewtype fromElem" $ do
    (fromElemTest
        (T.concat [ xmlHeader, "<TNewtype attr=\"attr txt\"/>" ])
        (TNewtype (TAttr (Attr "attr txt"))))
    (fromElemAll
        (genericFromElem optAll)
        (T.concat [ xmlHeader, "<TNewtype attr=\"attr txt\"/>" ])
        (TNewtype (TAttr (Attr "attr txt"))))

testNewtypeRoundTrip :: TestTree
testNewtypeRoundTrip = testProperty "TNewtype roundtrip"
                       (roundTrip :: TNewtype -> Bool)
