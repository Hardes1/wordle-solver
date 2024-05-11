{-# LANGUAGE InstanceSigs #-}
module Data.ColorError(ParseError(..)) where

data ParseError = InvalidColorLength | BadColorCharacter deriving Eq

instance Show ParseError where
    show :: ParseError -> String
    show InvalidColorLength = "Invalid color length!"
    show BadColorCharacter = "Bad color character!"