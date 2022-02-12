{-# options_ghc -fplugin=DerivingTH #-}

{-# language StandaloneDeriving #-}

module Main (main) where

import Control.Monad (unless)

import Foo

import System.Exit (die)


data Bar = Bar deriving Foo via template

data Baz = Baz
deriving via template instance Foo Baz


main :: IO ()
main = do
  unless (foo Bar) $ die "foo Bar"
  unless (foo Baz) $ die "foo Baz"
