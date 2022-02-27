{-# language TemplateHaskell #-}

module Foo (Foo(..)) where

import DerivingTH.Class

import Language.Haskell.TH


class Foo a where
  foo :: a -> Bool

instance DeriveTH Foo where
  deriveTH name = [d| instance Foo $(pure $ ConT name) where foo _ = True |]
  deriveTHType ty = [d| instance Foo $(pure ty) where foo _ = True |]
