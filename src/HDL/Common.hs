module HDL.Common where

import Clash.Prelude

gateMaybe :: Bool -> Maybe a -> Maybe a
gateMaybe True (Just x) = Just x
gateMaybe _ _ = Nothing

makeWriteTuple :: a -> Maybe b -> Maybe (a, b)
makeWriteTuple _ Nothing = Nothing
makeWriteTuple a (Just b) = Just (a, b)

resizeMaybe :: (KnownNat a, KnownNat b, Resize t) => Maybe (t a) -> Maybe (t b)
resizeMaybe a = resize <$> a
