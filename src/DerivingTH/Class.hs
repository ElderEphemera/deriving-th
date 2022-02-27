{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module DerivingTH.Class
  ( DeriveTH(deriveTH, deriveTHType), deriveTH', deriveTHType'
  ) where

import Control.Applicative (liftA2)

import Language.Haskell.TH
import Language.Haskell.TH.PprLib (nest, text, quotes, vcat, (<+>))


class DeriveTH (cls :: k) where
  {-# minimal deriveTH #-}

  deriveTH :: Name -> Q [Dec]

  -- | This method is used in the desugaring for standalone deriving.
  --
  -- The default implementation checks that the type is a type constructor
  -- applied to zero or more type variables (this must be the case when
  -- @-XFlexibleInstances@ is enabled). If it is then 'deriveTH' is called with
  -- the name of the type constructor. If it is not then an error is thrown.
  deriveTHType :: Type -> Q [Dec]
  deriveTHType ty = go ty where
    go (AppT f (VarT _)) = go f
    go (ConT name) = deriveTH @k @cls name
    go _ = fail . show $ vcat
      [ text "This TH deriving target does not support FlexibleInstances style types"
      , nest 4 $ text "The last instance type must be of the form (T a1 ... an) but it is"
        <+> quotes (ppr ty)
      ]

deriveTH' :: forall cls proxy. DeriveTH cls => proxy cls -> Name -> Q [Dec]
deriveTH' _ = deriveTH @_ @cls

deriveTHType' :: forall cls proxy. DeriveTH cls => proxy cls -> Q Exp -> Q [Dec]
deriveTHType' _ qexp = do
  (SigE _ ty) <- qexp
  deriveTHType @_ @cls ty


(&+) :: (a -> Q [Dec]) -> (a -> Q [Dec]) -> (a -> Q [Dec])
(&+) = liftA2 $ liftA2 (++)

instance
  ( DeriveTH a, DeriveTH b
  ) => DeriveTH (a, b) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c
  ) => DeriveTH (a, b, c) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d
  ) => DeriveTH (a, b, c, d) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  ) => DeriveTH (a, b, c, d, e) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f
  ) => DeriveTH (a, b, c, d, e, f) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g
  ) => DeriveTH (a, b, c, d, e, f, g) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h
  ) => DeriveTH (a, b, c, d, e, f, g, h) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i
  ) => DeriveTH (a, b, c, d, e, f, g, h, i) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j, k) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j, k, l) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l, DeriveTH m
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
    &+ deriveTH @_ @m
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l
    &+ deriveTHType @_ @m

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l, DeriveTH m, DeriveTH n
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
    &+ deriveTH @_ @m &+ deriveTH @_ @n
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l
    &+ deriveTHType @_ @m &+ deriveTHType @_ @n

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l, DeriveTH m, DeriveTH n, DeriveTH o
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
    &+ deriveTH @_ @m &+ deriveTH @_ @n &+ deriveTH @_ @o
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l
    &+ deriveTHType @_ @m &+ deriveTHType @_ @n &+ deriveTHType @_ @o


instance
  ( DeriveTH a, DeriveTH b
  ) => DeriveTH '(a, b) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c
  ) => DeriveTH '(a, b, c) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d
  ) => DeriveTH '(a, b, c, d) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  ) => DeriveTH '(a, b, c, d, e) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f
  ) => DeriveTH '(a, b, c, d, e, f) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g
  ) => DeriveTH '(a, b, c, d, e, f, g) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h
  ) => DeriveTH '(a, b, c, d, e, f, g, h) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j, k) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j, k, l) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l, DeriveTH m
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j, k, l, m) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
    &+ deriveTH @_ @m
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l
    &+ deriveTHType @_ @m

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l, DeriveTH m, DeriveTH n
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
    &+ deriveTH @_ @m &+ deriveTH @_ @n
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l
    &+ deriveTHType @_ @m &+ deriveTHType @_ @n

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l, DeriveTH m, DeriveTH n, DeriveTH o
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l
    &+ deriveTH @_ @m &+ deriveTH @_ @n &+ deriveTH @_ @o
  deriveTHType
    =  deriveTHType @_ @a &+ deriveTHType @_ @b &+ deriveTHType @_ @c
    &+ deriveTHType @_ @d &+ deriveTHType @_ @e &+ deriveTHType @_ @f
    &+ deriveTHType @_ @g &+ deriveTHType @_ @h &+ deriveTHType @_ @i
    &+ deriveTHType @_ @j &+ deriveTHType @_ @k &+ deriveTHType @_ @l
    &+ deriveTHType @_ @m &+ deriveTHType @_ @n &+ deriveTHType @_ @o
