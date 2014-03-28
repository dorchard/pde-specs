{-# LANGUAGE MultiParamTypeClasses, ImplicitParams, TypeOperators, GADTs, ConstraintKinds #-}

module Solvers where

import Types


class (Member d ds) => Euler t d ds where
    euler :: (Delta d a, Fractional a) => Dim d -> Dimension ds -> (t -> a) -> (t -> a)

instance Euler (Int, Int) X (X :. T :. Nil) where
    euler X _ h = \(x, t) -> (h (x + 1, t) - h (x, t)) / ?dx

instance Euler (Int, Int) T (X :. T :. Nil) where
    euler T _ h = \(x, t) -> (h (x, t + 1) - h (x, t)) / ?dt
