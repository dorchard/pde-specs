{-# LANGUAGE MultiParamTypeClasses, ImplicitParams, TypeOperators #-}

module Solvers where

import Types


class (Dim d, Member d ds, Fractional a) => Euler t a d ds where
    euler :: ds -> (t -> a) -> d -> (t -> a)

instance (Fractional a, ?dx :: a) => Euler (Int, Int) a X (X :. T :. Nil) where
    euler _ h X = \(x, t) -> (h (x + 1, t) - h (x, t)) / ?dx

instance (Fractional a, ?dt :: a) => Euler (Int, Int) a T (X :. T :. Nil) where
    euler _ h T = \(x, t) -> (h (x, t + 1) - h (x, t)) / ?dt
