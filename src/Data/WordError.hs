{-# LANGUAGE InstanceSigs #-}
module Data.WordError(Error(..)) where

data Error = InvalidLength | NotInLowerCase

instance Show Error where
    show :: Error -> String
    show InvalidLength = "Word should have length 5"
    show NotInLowerCase = "Some letters are not in lowercase"