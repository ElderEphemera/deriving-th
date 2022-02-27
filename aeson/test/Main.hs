{-# options_ghc -fplugin=DerivingTH #-}

{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Main (main) where

import Control.Monad ((<=<))

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy

import DerivingTH.Aeson

import Test.HUnit


data SimpleProd = SimpleProd Bool Char
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON) via template

data SimpleProd1 a = SimpleProd1 Bool a
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, ToJSON1, FromJSON1) via template

data SimpleProd2 a b = SimpleProd2 a b
  deriving stock (Eq, Show)
  deriving (ToJSON, ToJSON1, ToJSON2)
    via template
deriving via template instance FromJSON (SimpleProd2 a b)
deriving via template instance FromJSON1 (SimpleProd2 a)
deriving via template instance FromJSON2 SimpleProd2

newtype Named = NamedThingCONSTRName { namedThingFIELDName :: Bool }
  deriving stock (Eq, Show)
  deriving (JSONWith
    ( TagSingleConstructors
    , TaggedObject "constr" "_"
    , ModConstr (Strip "Named")
    , ModField (Strip "named")
    , Kebab
    )) via template

data Enumeration = EnumCONSTROne | EnumCONSTRTwo
  deriving stock (Eq, Show)
  deriving (JSONWith (NoAllNullaryToStringTag, Camel)) via template

data RecordA = RecordA { aMaybeChar :: Maybe Char, aBool :: Bool }
  deriving stock (Eq, Show)
  deriving (JSONWith (OmitNothingFields, Snake)) via template

newtype RecordB = RecordB { bRECChar :: Char }
  deriving stock (Eq, Show)
  deriving (JSONWith (RejectUnknownFields, Title)) via template

newtype RecordC = RecordC { cBool :: Bool }
  deriving stock (Eq, Show)
  deriving (JSONWith UnwrapUnaryRecords) via template

data SumA = SumABool Bool | SumAChar Char
  deriving stock (Eq, Show)
  deriving (JSONWith UntaggedValue) via template

data SumB = SumBBool Bool | SumBChar Char
  deriving stock (Eq, Show)
  deriving (JSONWith ObjectWithSingleField) via template

data SumC = SumCBool Bool | SumCChar Char
  deriving stock (Eq, Show)
  deriving (JSONWith TwoElemArray) via template


main :: IO ()
main = runTestTTAndExit $ test
  [ "orphans" ~: test
    [ "SimpleProd" ~: bidir (SimpleProd True 'X') "[true,\"X\"]"
    , "SimpleProd1" ~: test
      [ "JSON" ~: bidir (SimpleProd1 True 'X') "[true,\"X\"]"
      , "JSON1" ~: bidir1 (SimpleProd1 True 'X') "[true,\"X\"]"
      ]
    , "SimpleProd2" ~: test
      [ "JSON" ~: bidir (SimpleProd2 True 'X') "[true,\"X\"]"
      , "JSON1" ~: bidir1 (SimpleProd2 True 'X') "[true,\"X\"]"
      , "JSON2" ~: bidir2 (SimpleProd2 True 'X') "[true,\"X\"]"
      ]
    ]
  , "options" ~: test
    [ "NamedA" ~: bidir (NamedThingCONSTRName True)
      "{\"constr\":\"thing-constr-name\",\"thing-field-name\":true}"
    , "Enumeration" ~: bidir EnumCONSTROne "{\"tag\":\"enumConstrOne\"}"
    , "RecordA" ~: bidir (RecordA Nothing True) "{\"a_bool\":true}"
    , "RecordB" ~: test
      [ "success" ~: bidir (RecordB 'X') "{\"BRecChar\":\"X\"}"
      , "failure" ~: decode "{\"BRecChar\":\"X\",\"x\":0}" ~?= Nothing @RecordB
      ]
    , "RecordC" ~: bidir (RecordC True) "true"
    , "SumA" ~: bidir (SumABool True) "true"
    , "SumB" ~: bidir (SumBBool True) "{\"SumBBool\":true}"
    , "SumC" ~: bidir (SumCBool True) "[\"SumCBool\",true]"
    ]
  ]


bidir :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> ByteString -> Test
bidir = bidir' "" encode decode

bidir1
  :: (Eq (f a), Show (f a), ToJSON1 f, ToJSON a, FromJSON1 f, FromJSON a)
  => f a -> ByteString -> Test
bidir1 = bidir' "1" (encode . toJSON1) (parseMaybe parseJSON1 <=< decode)

bidir2
  :: ( Eq (f a b), Show (f a b)
     , ToJSON2 f, ToJSON a, ToJSON b, FromJSON2 f, FromJSON a, FromJSON b )
  => f a b -> ByteString -> Test
bidir2 = bidir' "2" (encode . toJSON2) (parseMaybe parseJSON2 <=< decode)

bidir'
  :: (Eq a, Eq b, Show a, Show b)
  => String -> (a -> b) -> (b -> Maybe a) -> a -> b -> Test
bidir' n en de x s = test
  [ "encode" ++ n ~: en x ~?= s
  , "decode" ++ n ~: de s ~?= Just x
  ]
