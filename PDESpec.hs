{-# LANGUAGE Rank2Types, TypeOperators, MultiParamTypeClasses, ExistentialQuantification, GADTs, UndecidableInstances, ConstraintKinds, NoMonomorphismRestriction, ImplicitParams #-}

module PDESpec(module Solvers, module Types, module PDESpec, module Output, module Text.CSV, module GHC.Arr) where

import System.Environment
import System.Exit

import Output
import Solvers
import Types
import Text.CSV

import GHC.Arr

{- Aliases to make writing specifications nicer -}

infix 2 ===
(===) = Equality

infixr 3 `withDomain`
withDomain = ($)
d     = Delta 1
d2    = Delta 2
d3    = Delta 3 

constant x = Constant x Nothing
varconst x = Constant undefined (Just x)

{- Helpers -}

type Unfix t = t -> t -- The type of an 'unfixed' function


{- Checking routine to compare an implementation with a spec -}

class Check ds t a where
    check :: Spec ds (t -> a) -> (t -> (a, a))

-- instance Check ds t a 
    checkEqn :: Dimension ds -> Eqn ds (t -> a) -> t -> a 

instance (?dx :: Float, ?dt :: Float) => Check (X :. T :. Nil) (Int, Int) Float where
    check (Equality e1 e2 ds) = \(x,t) -> (checkEqn ds e1 (x, t), checkEqn ds e2 (x, t))
    checkEqn ds (Constant a _)     = \(x, t) -> a
    checkEqn ds (Delta n impl dim) = (iterate (euler dim ds) impl) !! (fromInteger n)
    checkEqn ds (Times e1 e2)      = \(x, t) -> checkEqn ds e1 (x, t) * checkEqn ds e2 (x, t)
    checkEqn ds (Add e1 e2)        = \(x, t) -> checkEqn ds e1 (x, t) + checkEqn ds e2 (x, t)

data Model ds a where
    Model :: ((Indices ds t -> a) -> (Spec ds (Indices ds t -> a)))
          -> (Indices ds t -> a) 
          -> Model ds (Indices ds t -> a)

buildModelInterface :: String -> Model ds a -> IO ()
buildModelInterface name model = 
    do putStrLn $ "Model interface for " ++ name 
       putStrLn $ "What would you like to do? Options: "
       putStrLn $ "\t\t validate: compare specification with implementation"
       putStrLn $ "\t\t figure  : output figure with PDE and implementation results"
       choice <- getLine
       case choice of
         "validate" -> return ()
         "figure"   -> return ()
       
type Index ds a = (Show a, Ix a, Enum a, DeltaMap a ds)

