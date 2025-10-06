{-# LANGUAGE UndecidableInstances #-}
module Reshala.Ya.Instances where

import Ya
import Data.Eq (Eq)
import Data.Ord (Ord)

deriving newtype instance (Eq (f (Recursive f))) => Eq (Recursive f)
deriving stock instance (Eq i, Eq ii) => Eq (i `P` ii)
deriving stock instance (Eq i, Eq ii) => Eq (i `S` ii)
deriving newtype instance (Eq i, Eq ii) => Eq (T'I'II P i ii)
deriving newtype instance (Eq i, Eq ii) => Eq (T'I'II S i ii)
deriving newtype instance (Eq i, Eq ii) => Eq (T'II'I P ii i)
deriving newtype instance (Eq i, Eq ii) => Eq (T'II'I S ii i)
deriving newtype instance (Eq (t (tt i))) => Eq (T'TT'I t tt i)
deriving newtype instance (Eq i) => Eq (Construction Optional i)
deriving newtype instance (Eq i) => Eq (Tagged tag i)

deriving newtype instance (Ord (f (Recursive f))) => Ord (Recursive f)
deriving stock instance (Ord i, Ord ii) => Ord (i `P` ii)
deriving stock instance (Ord i, Ord ii) => Ord (i `S` ii)
deriving newtype instance (Ord i, Ord ii) => Ord (T'I'II P i ii)
deriving newtype instance (Ord i, Ord ii) => Ord (T'I'II S i ii)
deriving newtype instance (Ord i, Ord ii) => Ord (T'II'I P ii i)
deriving newtype instance (Ord i, Ord ii) => Ord (T'II'I S ii i)
deriving newtype instance (Ord (t (tt i))) => Ord (T'TT'I t tt i)
deriving newtype instance (Ord i) => Ord (Construction Optional i)
deriving newtype instance (Ord i) => Ord (Tagged tag i)
