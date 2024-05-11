{-# LANGUAGE InstanceSigs #-}
module Data.ColorError(ParseError(..)) where

data ParseError = InvalidColorLength | BadColorCharacter deriving Eq

instance Show ParseError where
    show :: ParseError -> String
    show InvalidColorLength = "Color should have length 5."
    show BadColorCharacter = "Color should only consist of alphabetical symbols."