{-# LANGUAGE InstanceSigs #-}
module Data.WordError(LogicError(..), ParseError(..)) where

data LogicError = UnknownWord deriving Eq

data ParseError = InvalidWordLength | BadWordCharacter deriving Eq

instance Show LogicError where
    show :: LogicError -> String
    show UnknownWord = "This word is not located in dictionaries."

instance Show ParseError where
    show :: ParseError -> String
    show InvalidWordLength = "Word should have length 5."
    show BadWordCharacter = "Word should only consist of alphabetical symbols."