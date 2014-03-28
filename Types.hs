{-# LANGUAGE GADTs, TypeOperators, EmptyDataDecls #-}

module Types where

data X = X deriving Show
data Y = Y deriving Show
data Z = Z deriving Show
data W = W deriving Show
data T = T deriving Show

class Dim d 
instance Dim X
instance Dim Y
instance Dim Z
instance Dim W
instance Dim T

data Nil
data t :. ts

data Dimension t where
    Nil :: Dimension Nil
    (:.) :: Dim d => d -> Dimension t -> Dimension (d :. t)
