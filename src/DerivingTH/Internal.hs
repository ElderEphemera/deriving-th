{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module DerivingTH.Internal
  ( deriveTH', deriveTHType', placeholder, Proxy(Proxy)
  ) where

import DerivingTH.Class (DeriveTH(deriveTH, deriveTHType))

import Data.Proxy (Proxy(Proxy))

import Language.Haskell.TH


deriveTH' :: forall cls proxy. DeriveTH cls => proxy cls -> Name -> Q [Dec]
deriveTH' _ = deriveTH @_ @cls

deriveTHType' :: forall cls proxy. DeriveTH cls => proxy cls -> Q Exp -> Q [Dec]
deriveTHType' _ qexp = do
  (SigE _ ty) <- qexp
  deriveTHType @_ @cls ty

placeholder :: a
placeholder = undefined
