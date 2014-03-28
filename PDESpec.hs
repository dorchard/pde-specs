{-# LANGUAGE Rank2Types, TypeOperators, MultiParamTypeClasses, ExistentialQuantification, GADTs, UndecidableInstances, ConstraintKinds, NoMonomorphismRestriction, ImplicitParams #-}

module PDESpec(module Solvers, module Types, module PDESpec) where

import Solvers
import Types

{-  Scaffolding for building equations
    Defines an abstract syntax tree for PDEs  -}

data Eqn ds a where
    Delta :: (Euler t d ds, Member d ds, Delta d a) => Int -> (t -> a) -> (Dim d) -> Eqn ds (t -> a)
    Times :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Add :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Minus :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Divide :: Eqn ds a -> Eqn ds a -> Eqn ds a
    Abs :: Eqn ds a -> Eqn ds a
    Constant :: ((Show a, Fractional a) => a) -> Eqn ds (t -> a)
             
instance (Show a, Fractional a) => Show (Eqn ds (t -> a)) where
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

instance (Show a, Fractional a) => Show (Spec ds (t -> a)) where
    show (Equality eq1 eq2 d) = "Equality (" ++ (show eq1) ++ ") (" ++ (show eq2) ++ " " ++ show d

{- Numerical classes -}

instance Num (Eqn ds (t -> a)) where
    a * b = Times a b
    a + b = Add a b
    a - b = Minus a b
    abs a = Abs a
    signum a = error "signum not implemented"
    fromInteger a = Constant (fromInteger a)

instance Fractional (Eqn ds (t -> a)) where
    fromRational x = Constant (fromRational x)

{- Aliases to make writing specifications nicer -}

infix 2 ===
(===) = Equality

infixr 3 `withDomain`
withDomain = ($)
d     = Delta 1
d2    = Delta 2
d3    = Delta 3 

{- Other helpers -}

type Unfix t = t -> t -- The type of an 'unfixed' function


{- Checking routine to compare an implementation with a spec -}

class Check ds t a where
    check :: Spec ds (t -> a) -> (t -> (a, a))

-- instance Check ds t a 
    checkEqn :: Dimension ds -> Eqn ds (t -> a) -> t -> a 

instance (?dx :: Float, ?dt :: Float) => Check (X :. T :. Nil) (Int, Int) Float where
    check (Equality e1 e2 ds) = \(x,t) -> (checkEqn ds e1 (x, t), checkEqn ds e2 (x, t))
    checkEqn ds (Constant a)       = \(x, t) -> a
    checkEqn ds (Delta n impl dim) = (iterate (euler dim ds) impl) !! n
    checkEqn ds (Times e1 e2)      = \(x, t) -> checkEqn ds e1 (x, t) * checkEqn ds e2 (x, t)
    checkEqn ds (Add e1 e2)        = \(x, t) -> checkEqn ds e1 (x, t) + checkEqn ds e2 (x, t)
