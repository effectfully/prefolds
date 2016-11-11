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

