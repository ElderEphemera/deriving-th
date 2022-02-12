{-# language AllowAmbiguousTypes #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module DerivingTH.Class where

import Language.Haskell.TH


class DeriveTH (cls :: k) where
  deriveTH :: Name -> Q [Dec]

deriveTH' :: forall cls proxy. DeriveTH cls => proxy cls -> Name -> Q [Dec]
deriveTH' _ = deriveTH @_ @cls
