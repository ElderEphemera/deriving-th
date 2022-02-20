{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module DerivingTH.Class
  ( DeriveTH(deriveTH), deriveTH'
  ) where

import Control.Applicative (liftA2)

import Language.Haskell.TH


class DeriveTH (cls :: k) where
  deriveTH :: Name -> Q [Dec]

deriveTH' :: forall cls proxy. DeriveTH cls => proxy cls -> Name -> Q [Dec]
deriveTH' _ = deriveTH @_ @cls


(&+) :: (Name -> Q [Dec]) -> (Name -> Q [Dec]) -> (Name -> Q [Dec])
(&+) = liftA2 $ liftA2 (++)

instance
  ( DeriveTH a, DeriveTH b
  ) => DeriveTH (a, b) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c
  ) => DeriveTH (a, b, c) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d
  ) => DeriveTH (a, b, c, d) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  ) => DeriveTH (a, b, c, d, e) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f
  ) => DeriveTH (a, b, c, d, e, f) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g
  ) => DeriveTH (a, b, c, d, e, f, g) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h
  ) => DeriveTH (a, b, c, d, e, f, g, h) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i
  ) => DeriveTH (a, b, c, d, e, f, g, h, i) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j, k) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l
  ) => DeriveTH (a, b, c, d, e, f, g, h, i, j, k, l) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l

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


instance
  ( DeriveTH a, DeriveTH b
  ) => DeriveTH '(a, b) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c
  ) => DeriveTH '(a, b, c) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d
  ) => DeriveTH '(a, b, c, d) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  ) => DeriveTH '(a, b, c, d, e) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f
  ) => DeriveTH '(a, b, c, d, e, f) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g
  ) => DeriveTH '(a, b, c, d, e, f, g) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h
  ) => DeriveTH '(a, b, c, d, e, f, g, h) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j, k) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k

instance
  ( DeriveTH a, DeriveTH b, DeriveTH c, DeriveTH d, DeriveTH e
  , DeriveTH f, DeriveTH g, DeriveTH h, DeriveTH i, DeriveTH j
  , DeriveTH k, DeriveTH l
  ) => DeriveTH '(a, b, c, d, e, f, g, h, i, j, k, l) where
  deriveTH
    =  deriveTH @_ @a &+ deriveTH @_ @b &+ deriveTH @_ @c &+ deriveTH @_ @d
    &+ deriveTH @_ @e &+ deriveTH @_ @f &+ deriveTH @_ @g &+ deriveTH @_ @h
    &+ deriveTH @_ @i &+ deriveTH @_ @j &+ deriveTH @_ @k &+ deriveTH @_ @l

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
