module HDL.Common where

import Clash.Prelude
import Data.Typeable
import Data.Char

gateMaybe :: Bool -> Maybe a -> Maybe a
gateMaybe True (Just x) = Just x
gateMaybe _ _ = Nothing

resizeMaybe :: (KnownNat a, KnownNat b, Resize t) => Maybe (t a) -> Maybe (t b)
resizeMaybe a = resize <$> a

tr :: (?doTrace :: Bool, BitPack a, NFDataX a, Typeable a, KnownDomain dom) => String -> Signal dom a -> Signal dom a
tr name = if ?doTrace then traceSignal name else id

embedLabel8 :: [Char] -> Unsigned 64
embedLabel8 = embedLabelHelper 8 0

embedLabelHelper :: Integer -> Unsigned 64 -> [Char] -> Unsigned 64
embedLabelHelper 0 res _  = res
embedLabelHelper n res [] = embedLabelHelper (n-1) (res*256) []
embedLabelHelper n res (hd:tl) = embedLabelHelper (n-1)
        ((res*256) + fromIntegral (ord hd)) tl