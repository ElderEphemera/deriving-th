module DerivingTH.Internal
  ( deriveTH', deriveTHType', placeholder, Proxy(Proxy)
  ) where

import DerivingTH.Class (deriveTH', deriveTHType')
import Data.Proxy (Proxy(Proxy))

placeholder :: a
placeholder = undefined
