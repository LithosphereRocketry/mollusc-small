module HDL.Common where

import Clash.Prelude
import Data.Typeable

gateMaybe :: Bool -> Maybe a -> Maybe a
gateMaybe True (Just x) = Just x
gateMaybe _ _ = Nothing

makeWriteTuple :: a -> Maybe b -> Maybe (a, b)
makeWriteTuple _ Nothing = Nothing
makeWriteTuple a (Just b) = Just (a, b)

resizeMaybe :: (KnownNat a, KnownNat b, Resize t) => Maybe (t a) -> Maybe (t b)
resizeMaybe a = resize <$> a

tr :: (?doTrace :: Bool, BitPack a, NFDataX a, Typeable a, KnownDomain dom) => String -> Signal dom a -> Signal dom a
tr name = if ?doTrace then traceSignal name else id

