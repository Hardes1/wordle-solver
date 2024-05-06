{-# LANGUAGE InstanceSigs #-}
module Data.WordError(Error(..)) where

data Error =  UnknownWord

instance Show Error where
    show :: Error -> String
    show UnknownWord = "This word is not located in dictionaries"