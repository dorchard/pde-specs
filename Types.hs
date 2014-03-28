{-# LANGUAGE GADTs, TypeOperators, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances, StandaloneDeriving, ConstraintKinds, TypeFamilies, ImplicitParams, RankNTypes #-}

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

{- Solver types -}

class (Member d ds) => Euler t d ds where
    euler :: (Delta d a, Fractional a) => Dim d -> Dimension ds -> (t -> a) -> (t -> a)

{-  Scaffolding for building equations
    Defines an abstract syntax tree for PDEs  -}

data Eqn ds a where
    Delta :: (Euler t d ds, Member d ds, Delta d a) => Integer -> (t -> a) -> (Dim d) -> Eqn ds (t -> a)
    Times :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Add :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Minus :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Divide :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Abs :: Eqn ds a -> Eqn ds a
    Constant :: ((Show a) => a) -> Eqn ds (t -> a)
             
instance (Show a) => Show (Eqn ds (t -> a)) where
    show (Delta n _ dim) = "Delta " ++ (show n) ++ " " ++ (show dim) ++ " fun "
    show (Times e1 e2)   = "Times (" ++ show e1 ++ ") (" ++  show e2 ++ ")"
    show (Add e1 e2)     = "Add (" ++ show e1 ++ ") (" ++  show e2 ++ ")"
    show (Minus e1 e2)   = "Minus (" ++ show e1 ++ ") (" ++  show e2 ++ ")"
    show (Divide e1 e2)  = "Divide (" ++ show e1 ++ ") (" ++  show e2 ++ ")"
    show (Abs e)         = "Abs (" ++ show e ++ ")"
    show (Constant a)    = "Constant " ++ show a 

-- Specification type (currently just one equality)
-- TODO: could be more in the future

data Spec ds a = Equality (Eqn ds a) (Eqn ds a) (Dimension ds) 

lhs, rhs :: Spec ds a -> Eqn ds a
lhs (Equality l _ _) = l
rhs (Equality _ r _) = r

instance (Show a) => Show (Spec ds (t -> a)) where
    show (Equality eq1 eq2 d) = "Equality (" ++ (show eq1) ++ ") (" ++ (show eq2) ++ " " ++ show d

{- Numerical classes -}

instance Num a => Num (Eqn ds (t -> a)) where
    a * b = Times a b
    a + b = Add a b
    a - b = Minus a b
    abs a = Abs a
    signum a = error "signum not implemented"
    fromInteger a = Constant (fromInteger a)

instance Fractional a => Fractional (Eqn ds (t -> a)) where
    fromRational x = Constant (fromRational x)
