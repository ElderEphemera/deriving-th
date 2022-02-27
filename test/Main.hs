{-# options_ghc -fplugin=DerivingTH #-}

{-# language FlexibleInstances #-}
{-# language StandaloneDeriving #-}

module Main (main) where

import Control.Monad (unless)

import Foo

import System.Exit (die)


data Bar = Bar deriving Foo via template

data Baz = Baz
deriving via template instance Foo Baz

data Bay a = Bay
deriving via template instance Foo (Bay a)

data Bam a = Bam a
deriving via template instance Foo (Bam Char)


main :: IO ()
main = do
  unless (foo Bar) $ die "foo Bar"
  unless (foo Baz) $ die "foo Baz"
  unless (foo Bay) $ die "foo Bay"
  unless (foo $ Bam 'x') $ die "foo Bam"
