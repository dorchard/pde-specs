{-# LANGUAGE GADTs, TypeOperators, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances, StandaloneDeriving, ConstraintKinds, TypeFamilies, ImplicitParams #-}

{- Declares various types that are used everywhere -}

module Types where

import GHC.Prim

{- Type-level dimension information -}

data X
data Y
data Z
data T

data Dim t where
    X :: Dim X
    Y :: Dim Y
    Z :: Dim Z
    T :: Dim T

{- Mapping from dimensions to their 'implicit parameter' requirements -}

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

{- Map type-level dimension lists to tuples #-}

type family Indices ds t
type instance Indices Nil t = ()
type instance Indices (a :. Nil) t = (t)
type instance Indices (a :. b :. Nil) t = (t, t)
type instance Indices (a :. b :. c :. Nil) t = (t, t, t)
type instance Indices (a :. b :. c :. d :. Nil) t = (t, t, t, t)