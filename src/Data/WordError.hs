{-# LANGUAGE InstanceSigs #-}
module Data.WordError(Error(..)) where

data Error = InvalidLength | NotInUpperCase

instance Show Error where
    show :: Error -> String
    show InvalidLength = "Word should have length 5"
    show NotInUpperCase = "Some letters are not in upeercase"