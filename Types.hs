{-# LANGUAGE GADTs, TypeOperators, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances, StandaloneDeriving, ConstraintKinds, TypeFamilies, ImplicitParams #-}

module Types where

import GHC.Prim

-- data Dim = X | Y | Z | T deriving Show

data X
data Y
data Z
data T

data Dim t where
    X :: Dim X
    Y :: Dim Y
    Z :: Dim Z
    T :: Dim T

type family Delta t a :: Constraint
type instance Delta X a = ?dx :: a
type instance Delta Y a = ?dy :: a
type instance Delta Z a = ?dz :: a
type instance Delta T a = ?dt :: a

deriving instance Show (Dim t)

data Nil
data t :. ts

infixr 3 :.

data Dimension t where
    Nil :: Dimension Nil
    (:.) :: Dim d -> Dimension t -> Dimension (d :. t)

instance Show (Dimension t) where
    show Nil = "Nil"
    show (d :. ds) = (show d) ++ ":." ++ (show ds)

class Member t ts 
instance Member t (t :. ts) 
instance Member t ts => Member t (t' :. ts)
