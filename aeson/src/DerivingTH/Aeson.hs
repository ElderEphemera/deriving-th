{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

-- | This module provides orphan instances and wrappers for interfacing the
-- deriving-th package with aeson.
module DerivingTH.Aeson (
  -- * Deriving Targets
    JSON, JSON1, JSON2
  -- ** Customizable Deriving Targets
  -- *** Bundles
  , JSONWith, JSON1With, JSON2With
  -- *** ToJSON
  , ToJSONWith, ToJSON1With, ToJSON2With
  -- *** FromJSON
  , FromJSONWith, FromJSON1With, FromJSON2With

  -- * Deriving Options
  , JSONOption(applyJSONOption)
  -- ** String Options
  , ModField, ModConstr, ModAll
  -- ** Sum Encoding Options
  , TaggedObject, UntaggedValue, ObjectWithSingleField, TwoElemArray
  -- ** Boolean Options
  , NoAllNullaryToStringTag, OmitNothingFields, UnwrapUnaryRecords
  , TagSingleConstructors, RejectUnknownFields
  -- *** Reverse Boolean Options
  -- | These options set their values to the default so on their own they do
  -- nothing. But they may be useful if you have some standard options that you
  -- want to override for specific types.
  , AllNullaryToStringTag, NoOmitNothingFields, NoUnwrapUnaryRecords
  , NoTagSingleConstructors, NoRejectUnknownFields

  -- * String Conversions
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

-- | A TH deriving target for 'Data.Aeson.ToJSON' using @options@. If you just
-- want 'Data.Aeson.defaultOptions' you can use 'Data.Aeson.ToJSON' instead.
data ToJSONWith options
instance JSONOption options => DeriveTH (ToJSONWith options) where
  deriveTH = deriveToJSON $ applyJSONOption @options defaultOptions

-- | A TH deriving target for 'Data.Aeson.ToJSON1' using @options@. If you just
-- want 'Data.Aeson.defaultOptions' you can use 'Data.Aeson.ToJSON1' instead.
data ToJSON1With options
instance JSONOption options => DeriveTH (ToJSON1With options) where
  deriveTH = deriveToJSON1 $ applyJSONOption @options defaultOptions

-- | A TH deriving target for 'Data.Aeson.ToJSON2' using @options@. If you just
-- want 'Data.Aeson.defaultOptions' you can use 'Data.Aeson.ToJSON2' instead.
data ToJSON2With options
instance JSONOption options => DeriveTH (ToJSON2With options) where
  deriveTH = deriveToJSON2 $ applyJSONOption @options defaultOptions


-- | A TH deriving target for 'Data.Aeson.FromJSON' using @options@. If you just
-- want 'Data.Aeson.defaultOptions' you can use 'Data.Aeson.FromJSON' instead.
data FromJSONWith options
instance JSONOption options => DeriveTH (FromJSONWith options) where
  deriveTH = deriveFromJSON $ applyJSONOption @options defaultOptions

-- | A TH deriving target for 'Data.Aeson.FromJSON1' using @options@. If you
-- just want 'Data.Aeson.defaultOptions' you can use 'Data.Aeson.FromJSON1'
-- instead.
data FromJSON1With options
instance JSONOption options => DeriveTH (FromJSON1With options) where
  deriveTH = deriveFromJSON1 $ applyJSONOption @options defaultOptions

-- | A TH deriving target for 'Data.Aeson.FromJSON2' using @options@. If you
-- just want 'Data.Aeson.defaultOptions' you can use 'Data.Aeson.FromJSON2'
-- instead.
data FromJSON2With options
instance JSONOption options => DeriveTH (FromJSON2With options) where
  deriveTH = deriveFromJSON2 $ applyJSONOption @options defaultOptions


-- | A TH deriving target for 'Data.Aeson.ToJSON' and 'Data.Aeson.FromJSON'
-- using @options@. If you just want 'Data.Aeson.defaultOptions' you can use
-- 'JSON' instead.
type JSONWith options = (ToJSONWith options, FromJSONWith options)

-- | A TH deriving target for 'Data.Aeson.ToJSON1' and 'Data.Aeson.FromJSON1'
-- using @options@. If you just want 'Data.Aeson.defaultOptions' you can use
-- 'JSON1' instead.
type JSON1With options = (ToJSON1With options, FromJSON1With options)

-- | A TH deriving target for 'Data.Aeson.ToJSON2' and 'Data.Aeson.FromJSON2'
-- using @options@. If you just want 'Data.Aeson.defaultOptions' you can use
-- 'JSON2' instead.
type JSON2With options = (ToJSON2With options, FromJSON2With options)


-- | Deriving 'Data.Aeson.ToJSON' using 'Data.Aeson.defaultOptions'. If you need
-- custom options, use 'ToJSONWith'.
instance DeriveTH ToJSON where
  deriveTH = deriveToJSON defaultOptions

-- | Deriving 'Data.Aeson.ToJSON1' using 'Data.Aeson.defaultOptions'. If you
-- need custom options, use 'ToJSON1With'.
instance DeriveTH ToJSON1 where
  deriveTH = deriveToJSON1 defaultOptions

-- | Deriving 'Data.Aeson.ToJSON2' using 'Data.Aeson.defaultOptions'. If you
-- need custom options, use 'ToJSON2With'.
instance DeriveTH ToJSON2 where
  deriveTH = deriveToJSON2 defaultOptions


-- | Deriving 'Data.Aeson.FromJSON' using 'Data.Aeson.defaultOptions'. If you
-- need custom options, use 'FromJSONWith'.
instance DeriveTH FromJSON where
  deriveTH = deriveFromJSON defaultOptions

-- | Deriving 'Data.Aeson.FromJSON1' using 'Data.Aeson.defaultOptions'. If you
-- need custom options, use 'FromJSON1With'.
instance DeriveTH FromJSON1 where
  deriveTH = deriveFromJSON1 defaultOptions

-- | Deriving 'Data.Aeson.FromJSON2' using 'Data.Aeson.defaultOptions'. If you
-- need custom options, use 'FromJSON2With'.
instance DeriveTH FromJSON2 where
  deriveTH = deriveFromJSON2 defaultOptions


-- | A TH deriving target for 'Data.Aeson.ToJSON' and 'Data.Aeson.FromJSON'
-- using 'Data.Aeson.defaultOptions'. If you need custom options, use
-- 'JSONWith'.
type JSON = JSONWith ()

-- | A TH deriving target for 'Data.Aeson.ToJSON1' and 'Data.Aeson.FromJSON1'
-- using 'Data.Aeson.defaultOptions'. If you need custom options, use
-- 'JSON1With'.
type JSON1 = JSON1With ()

-- | A TH deriving target for 'Data.Aeson.ToJSON2' and 'Data.Aeson.FromJSON2'
-- using 'Data.Aeson.defaultOptions'. If you need custom options, use
-- 'JSON2With'.
type JSON2 = JSON2With ()


-- | A type that can be used with 'JSONWith' and the other customizable deriving
-- targets.
--
-- The tuple instances apply options in order with string options compounding on
-- each other and sum encoding and boolean options overriding previous options
-- for the same field.
--
-- The @()@ instance does nothing so using only it will result in using
-- 'Data.Aeson.defaultOptions'.
class JSONOption a where
  applyJSONOption :: Options -> Options

-- | Use @conversion@ for 'Data.Aeson.fieldLabelModifier'.
data ModField conversion
instance StringConversion conversion => JSONOption (ModField conversion) where
  applyJSONOption options = options
    { fieldLabelModifier =
      convertString @conversion . fieldLabelModifier options
    }

-- | Use @conversion@ for 'Data.Aeson.constructorTagModifier'.
data ModConstr conversion
instance StringConversion conversion => JSONOption (ModConstr conversion) where
  applyJSONOption options = options
    { constructorTagModifier =
      convertString @conversion . constructorTagModifier options
    }

-- | Use @conversion@ for both 'Data.Aeson.fieldLabelModifier' and
-- 'Data.Aeson.constructorTagModifier'.
--
-- When deriving using string conversions from this library you should never
-- need to use this, as you can just use the string conversions directly.
-- However when creating your own conversions, this can be useful for deriving
-- 'JSONOption' as so:
--
-- @
--     data MyConversion deriving JSONOption via ModAll MyConversion
--
--     instance StringConversion MyConversion where
--       convertString = ...
-- @
type ModAll conversion = (ModField conversion, ModConstr conversion)

-- | Set 'Data.Aeson.sumEncoding' to @'Data.Aeson.TaggedObject' tagFieldName
-- contentsFieldName@.
data TaggedObject (tagFieldName :: Symbol) (contentsFieldName :: Symbol)
instance (KnownSymbol tagFieldName, KnownSymbol contentsFieldName)
  => JSONOption (TaggedObject tagFieldName contentsFieldName) where
  applyJSONOption options = options
    { sumEncoding = TaggedObject
      { tagFieldName = symbolVal @tagFieldName Proxy
      , contentsFieldName = symbolVal @contentsFieldName Proxy
      }
    }

-- | Set 'Data.Aeson.sumEncoding' to 'Data.Aeson.UntaggedValue'.
data UntaggedValue
instance JSONOption UntaggedValue where
  applyJSONOption options = options { sumEncoding = UntaggedValue }

-- | Set 'Data.Aeson.sumEncoding' to 'Data.Aeson.ObjectWithSingleField'.
data ObjectWithSingleField
instance JSONOption ObjectWithSingleField where
  applyJSONOption options = options { sumEncoding = ObjectWithSingleField }

-- | Set 'Data.Aeson.sumEncoding' to 'Data.Aeson.TwoElemArray'.
data TwoElemArray
instance JSONOption TwoElemArray where
  applyJSONOption options = options { sumEncoding = TwoElemArray }


-- | Reset 'allNullaryToStringTag' to 'True', which is the default.
data AllNullaryToStringTag
instance JSONOption AllNullaryToStringTag where
  applyJSONOption options = options { allNullaryToStringTag = True }

-- | Set 'Data.Aeson.allNullaryToStringTag' to 'False'.
data NoAllNullaryToStringTag
instance JSONOption NoAllNullaryToStringTag where
  applyJSONOption options = options { allNullaryToStringTag = False }

-- | Set 'Data.Aeson.omitNothingFields' to 'True'.
data OmitNothingFields
instance JSONOption OmitNothingFields where
  applyJSONOption options = options { omitNothingFields = True }

-- | Reset 'Data.Aeson.omitNothingFields' to 'False', which is the default.
data NoOmitNothingFields
instance JSONOption NoOmitNothingFields where
  applyJSONOption options = options { omitNothingFields = False }

-- | Set 'Data.Aeson.unwrapUnaryRecords' to 'True'.
data UnwrapUnaryRecords
instance JSONOption UnwrapUnaryRecords where
  applyJSONOption options = options { unwrapUnaryRecords = True }

-- | Reset 'Data.Aeson.unwrapUnaryRecords' to 'False', which is the default.
data NoUnwrapUnaryRecords
instance JSONOption NoUnwrapUnaryRecords where
  applyJSONOption options = options { unwrapUnaryRecords = False }

-- | Set 'Data.Aeson.tagSingleConstructors' to 'True'.
data TagSingleConstructors
instance JSONOption TagSingleConstructors where
  applyJSONOption options = options { tagSingleConstructors = True }

-- | Reset 'Data.Aeson.tagSingleConstructors' to 'False', which is the default.
data NoTagSingleConstructors
instance JSONOption NoTagSingleConstructors where
  applyJSONOption options = options { tagSingleConstructors = False }

-- | Set 'Data.Aeson.rejectUnknownFields' to 'True'.
data RejectUnknownFields
instance JSONOption RejectUnknownFields where
  applyJSONOption options = options { rejectUnknownFields = True }

-- | Reset 'Data.Aeson.rejectUnknownFields' to 'False', which is the default.
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


-- | A type representing string conversions to be used with 'ModField',
-- 'ModConstr', and 'ModAll'.
--
-- Ideally, all types with an instance for this type class should have an
-- instance for 'JSONOption' similar to the one for 'ModAll' as well. This can
-- be done easily derived as explained in the documentation for 'ModAll'.
class StringConversion a where
  convertString :: String -> String

breakWords :: String -> [String]
breakWords = words . camelTo2 ' '

onFirst :: (a -> a) -> [a] -> [a]
onFirst _ [] = []
onFirst f (x:xs) = f x : xs

-- | Convert strings into kebab-case. Word boundaries are decided just like
-- 'Data.Aeson.camelTo2'.
data Kebab deriving JSONOption via ModAll Kebab
instance StringConversion Kebab where
  convertString = camelTo2 '-'

-- | Convert strings into camelCase. Word boundaries are decided just like
-- 'Data.Aeson.camelTo2'.
data Camel deriving JSONOption via ModAll Camel
instance StringConversion Camel where
  convertString = onFirst toLower . convertString @Title

-- | Convert strings into snake_case. Word boundaries are decided just like
-- 'Data.Aeson.camelTo2'.
data Snake deriving JSONOption via ModAll Snake
instance StringConversion Snake where
  convertString = camelTo2 '_'

-- | Convert strings into TitleCase. Word boundaries are decided just like
-- 'Data.Aeson.camelTo2'.
data Title deriving JSONOption via ModAll Title
instance StringConversion Title where
  convertString = concat . map (onFirst toUpper) . breakWords

-- | Strip @prefix@ from the string. If the string does not start with the
-- @prefix@ then an error is thrown.
data Strip (prefix :: Symbol) deriving JSONOption via ModAll (Strip prefix)
instance KnownSymbol prefix => StringConversion (Strip prefix) where
  convertString str =
    let prefix = symbolVal @prefix Proxy
    in case stripPrefix prefix str of
      Just rest -> rest
      Nothing -> error $ "The name '" ++ str
        ++ "' does not start with prefix '" ++ prefix ++ "'"
