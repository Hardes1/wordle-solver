{-# LANGUAGE InstanceSigs #-}
module Data.WordError(Error(..)) where

data Error = InvalidLength Int | BadCharacter

instance Show Error where
    show :: Error -> String
    show (InvalidLength len) = "Word should have length 5. Your word has length" <> show len <> "."
    show BadCharacter = "Word should only consist of alphabetical symbols."