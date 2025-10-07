module Reshala.Ya.Conversion where

import Ya

import Data.Ord (Ord)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

find :: Ord k => k `AR____` Map.Map k v `AR___` Stops k v
find k m = case Map.lookup k m of
 Maybe.Nothing -> Error k
 Maybe.Just x -> Valid x
