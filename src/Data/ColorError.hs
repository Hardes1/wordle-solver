{-# LANGUAGE InstanceSigs #-}
module Data.ColorError(ParseError(..)) where

data ParseError = InvalidColorLength | BadColorCharacter

instance Show ParseError where
    show :: ParseError -> String
    show InvalidColorLength = "Invalid color length!"
    show BadColorCharacter = "Bad color character!"