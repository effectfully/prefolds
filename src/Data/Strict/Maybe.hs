module Data.Strict.Maybe where

data Maybe' a = Just' !a | Nothing'

lazy :: Maybe' a -> Maybe a
lazy  Nothing' = Nothing
lazy (Just' a) = Just a
{-# INLINABLE lazy #-}

strict :: Maybe a -> Maybe' a
strict  Nothing = Nothing'
strict (Just a) = Just' a
{-# INLINABLE strict #-}

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' y f  Nothing' = y
maybe' y f (Just' x) = f x
{-# INLINABLE maybe' #-}
