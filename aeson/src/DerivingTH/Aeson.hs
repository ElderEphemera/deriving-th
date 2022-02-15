{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module DerivingTH.Aeson
  ( JSON, JSON1, JSON2
  , JSONWith, JSON1With, JSON2With
  , ToJSONWith, ToJSON1With, ToJSON2With
  , FromJSONWith, FromJSON1With, FromJSON2With
  , JSONOption(applyJSONOption)
  , ModField, ModConstr, ModAll
  , TaggedObject, UntaggedValue, ObjectWithSingleField, TwoElemArray
  , NoAllNullaryToStringTag, OmitNothingFields, UnwrapUnaryRecords
  , TagSingleConstructors, RejectUnknownFields
  , AllNullaryToStringTag, NoOmitNothingFields, NoUnwrapUnaryRecords
  , NoTagSingleConstructors, NoRejectUnknownFields
  , StringConversion(convertString)
  , Kebab, Camel, Snake, Title, Strip
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower, toUpper)
import Data.List (stripPrefix)
import Data.Proxy (Proxy(Proxy))

import DerivingTH.Class

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)


data ToJSONWith option
instance JSONOption option => DeriveTH (ToJSONWith option) where
  deriveTH = deriveToJSON $ applyJSONOption @option defaultOptions

data ToJSON1With option
instance JSONOption option => DeriveTH (ToJSON1With option) where
  deriveTH = deriveToJSON1 $ applyJSONOption @option defaultOptions

data ToJSON2With option
instance JSONOption option => DeriveTH (ToJSON2With option) where
  deriveTH = deriveToJSON2 $ applyJSONOption @option defaultOptions


data FromJSONWith option
instance JSONOption option => DeriveTH (FromJSONWith option) where
  deriveTH = deriveFromJSON $ applyJSONOption @option defaultOptions

data FromJSON1With option
instance JSONOption option => DeriveTH (FromJSON1With option) where
  deriveTH = deriveFromJSON1 $ applyJSONOption @option defaultOptions

data FromJSON2With option
instance JSONOption option => DeriveTH (FromJSON2With option) where
  deriveTH = deriveFromJSON2 $ applyJSONOption @option defaultOptions


type JSONWith option = (ToJSONWith option, FromJSONWith option)
type JSON1With option = (ToJSON1With option, FromJSON1With option)
type JSON2With option = (ToJSON2With option, FromJSON2With option)


instance DeriveTH ToJSON where
  deriveTH = deriveToJSON defaultOptions

instance DeriveTH ToJSON1 where
  deriveTH = deriveToJSON1 defaultOptions

instance DeriveTH ToJSON2 where
  deriveTH = deriveToJSON2 defaultOptions


instance DeriveTH FromJSON where
  deriveTH = deriveFromJSON defaultOptions

instance DeriveTH FromJSON1 where
  deriveTH = deriveFromJSON1 defaultOptions

instance DeriveTH FromJSON2 where
  deriveTH = deriveFromJSON2 defaultOptions


type JSON = JSONWith ()
type JSON1 = JSON1With ()
type JSON2 = JSON2With ()


class JSONOption a where
  applyJSONOption :: Options -> Options

data ModField conversion
instance StringConversion conversion => JSONOption (ModField conversion) where
  applyJSONOption options = options
    { fieldLabelModifier =
      convertString @conversion . fieldLabelModifier options
    }

data ModConstr conversion
instance StringConversion conversion => JSONOption (ModConstr conversion) where
  applyJSONOption options = options
    { constructorTagModifier =
      convertString @conversion . constructorTagModifier options
    }

type ModAll conversion = (ModField conversion, ModConstr conversion)

data TaggedObject (tagFieldName :: Symbol) (contentsFieldName :: Symbol)
instance (KnownSymbol tagFieldName, KnownSymbol contentsFieldName)
  => JSONOption (TaggedObject tagFieldName contentsFieldName) where
  applyJSONOption options = options
    { sumEncoding = TaggedObject
      { tagFieldName = symbolVal @tagFieldName Proxy
      , contentsFieldName = symbolVal @contentsFieldName Proxy
      }
    }

data UntaggedValue
instance JSONOption UntaggedValue where
  applyJSONOption options = options { sumEncoding = UntaggedValue }

data ObjectWithSingleField
instance JSONOption ObjectWithSingleField where
  applyJSONOption options = options { sumEncoding = ObjectWithSingleField }

data TwoElemArray
instance JSONOption TwoElemArray where
  applyJSONOption options = options { sumEncoding = TwoElemArray }


data AllNullaryToStringTag
instance JSONOption AllNullaryToStringTag where
  applyJSONOption options = options { allNullaryToStringTag = True }

data NoAllNullaryToStringTag
instance JSONOption NoAllNullaryToStringTag where
  applyJSONOption options = options { allNullaryToStringTag = False }

data OmitNothingFields
instance JSONOption OmitNothingFields where
  applyJSONOption options = options { omitNothingFields = True }

data NoOmitNothingFields
instance JSONOption NoOmitNothingFields where
  applyJSONOption options = options { omitNothingFields = False }

data UnwrapUnaryRecords
instance JSONOption UnwrapUnaryRecords where
  applyJSONOption options = options { unwrapUnaryRecords = True }

data NoUnwrapUnaryRecords
instance JSONOption NoUnwrapUnaryRecords where
  applyJSONOption options = options { unwrapUnaryRecords = False }

data TagSingleConstructors
instance JSONOption TagSingleConstructors where
  applyJSONOption options = options { tagSingleConstructors = True }

data NoTagSingleConstructors
instance JSONOption NoTagSingleConstructors where
  applyJSONOption options = options { tagSingleConstructors = False }

data RejectUnknownFields
instance JSONOption RejectUnknownFields where
  applyJSONOption options = options { rejectUnknownFields = True }

data NoRejectUnknownFields
instance JSONOption NoRejectUnknownFields where
  applyJSONOption options = options { rejectUnknownFields = False }

instance JSONOption () where
  applyJSONOption = id

instance
  ( JSONOption a, JSONOption b
  ) => JSONOption (a, b) where
  applyJSONOption
    = applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c
  ) => JSONOption (a, b, c) where
  applyJSONOption
    = applyJSONOption @c . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d
  ) => JSONOption (a, b, c, d) where
  applyJSONOption
    = applyJSONOption @d . applyJSONOption @c . applyJSONOption @b
    . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  ) => JSONOption (a, b, c, d, e) where
  applyJSONOption
    = applyJSONOption @e . applyJSONOption @d . applyJSONOption @c
    . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f
  ) => JSONOption (a, b, c, d, e, f) where
  applyJSONOption
    = applyJSONOption @f . applyJSONOption @e . applyJSONOption @d
    . applyJSONOption @c . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g
  ) => JSONOption (a, b, c, d, e, f, g) where
  applyJSONOption
    = applyJSONOption @g . applyJSONOption @f . applyJSONOption @e
    . applyJSONOption @d . applyJSONOption @c . applyJSONOption @b
    . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h
  ) => JSONOption (a, b, c, d, e, f, g, h) where
  applyJSONOption
    = applyJSONOption @h . applyJSONOption @g . applyJSONOption @f
    . applyJSONOption @e . applyJSONOption @d . applyJSONOption @c
    . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h, JSONOption i
  ) => JSONOption (a, b, c, d, e, f, g, h, i) where
  applyJSONOption
    = applyJSONOption @i . applyJSONOption @h . applyJSONOption @g
    . applyJSONOption @f . applyJSONOption @e . applyJSONOption @d
    . applyJSONOption @c . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h, JSONOption i, JSONOption j
  ) => JSONOption (a, b, c, d, e, f, g, h, i, j) where
  applyJSONOption
    = applyJSONOption @j . applyJSONOption @i . applyJSONOption @h
    . applyJSONOption @g . applyJSONOption @f . applyJSONOption @e
    . applyJSONOption @d . applyJSONOption @c . applyJSONOption @b
    . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h, JSONOption i, JSONOption j
  , JSONOption k
  ) => JSONOption (a, b, c, d, e, f, g, h, i, j, k) where
  applyJSONOption
    = applyJSONOption @k . applyJSONOption @j . applyJSONOption @i
    . applyJSONOption @h . applyJSONOption @g . applyJSONOption @f
    . applyJSONOption @e . applyJSONOption @d . applyJSONOption @c
    . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h, JSONOption i, JSONOption j
  , JSONOption k, JSONOption l
  ) => JSONOption (a, b, c, d, e, f, g, h, i, j, k, l) where
  applyJSONOption
    = applyJSONOption @l . applyJSONOption @k . applyJSONOption @j
    . applyJSONOption @i . applyJSONOption @h . applyJSONOption @g
    . applyJSONOption @f . applyJSONOption @e . applyJSONOption @d
    . applyJSONOption @c . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h, JSONOption i, JSONOption j
  , JSONOption k, JSONOption l, JSONOption m
  ) => JSONOption (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  applyJSONOption
    = applyJSONOption @m . applyJSONOption @l . applyJSONOption @k
    . applyJSONOption @j . applyJSONOption @i . applyJSONOption @h
    . applyJSONOption @g . applyJSONOption @f . applyJSONOption @e
    . applyJSONOption @d . applyJSONOption @c . applyJSONOption @b
    . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h, JSONOption i, JSONOption j
  , JSONOption k, JSONOption l, JSONOption m, JSONOption n
  ) => JSONOption (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  applyJSONOption
    = applyJSONOption @n . applyJSONOption @m . applyJSONOption @l
    . applyJSONOption @k . applyJSONOption @j . applyJSONOption @i
    . applyJSONOption @h . applyJSONOption @g . applyJSONOption @f
    . applyJSONOption @e . applyJSONOption @d . applyJSONOption @c
    . applyJSONOption @b . applyJSONOption @a

instance
  ( JSONOption a, JSONOption b, JSONOption c, JSONOption d, JSONOption e
  , JSONOption f, JSONOption g, JSONOption h, JSONOption i, JSONOption j
  , JSONOption k, JSONOption l, JSONOption m, JSONOption n, JSONOption o
  ) => JSONOption (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  applyJSONOption
    = applyJSONOption @o . applyJSONOption @n . applyJSONOption @m
    . applyJSONOption @l . applyJSONOption @k . applyJSONOption @j
    . applyJSONOption @i . applyJSONOption @h . applyJSONOption @g
    . applyJSONOption @f . applyJSONOption @e . applyJSONOption @d
    . applyJSONOption @c . applyJSONOption @b . applyJSONOption @a


class StringConversion a where
  convertString :: String -> String

breakWords :: String -> [String]
breakWords = words . camelTo2 ' '

onFirst :: (a -> a) -> [a] -> [a]
onFirst _ [] = []
onFirst f (x:xs) = f x : xs

data Kebab deriving JSONOption via ModAll Kebab
instance StringConversion Kebab where
  convertString = camelTo2 '-'

data Camel deriving JSONOption via ModAll Camel
instance StringConversion Camel where
  convertString = onFirst toLower . convertString @Title

data Snake deriving JSONOption via ModAll Snake
instance StringConversion Snake where
  convertString = camelTo2 '_'

data Title deriving JSONOption via ModAll Title
instance StringConversion Title where
  convertString = concat . map (onFirst toUpper) . breakWords

data Strip (prefix :: Symbol) deriving JSONOption via ModAll (Strip prefix)
instance KnownSymbol prefix => StringConversion (Strip prefix) where
  convertString str =
    let prefix = symbolVal @prefix Proxy
    in case stripPrefix prefix str of
      Just rest -> rest
      Nothing -> error $ "Then name '" ++ str
        ++ "' does not start with prefix '" ++ prefix ++ "'"
