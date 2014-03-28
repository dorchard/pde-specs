{-# LANGUAGE ImplicitParams #-}

import Data.Function.ArrayMemoize

import Plot
import PDESpec

spec h alpha = ((d h T) === (alpha * (d2 h X)))
                  `withDomain` (X :. T :. Nil)

impl alpha = 
         let
            h :: Unfix ((Int, Int) -> Float)
            h h' (x, t)
              | x == 0   = 1
              | x == ?nx = h' (x - 1, t)
              | t == 0   = 0
              | otherwise =  h' (x, t-1) + r * (h' (x+1, t-1) - 2 * h' (x, t-1) + h' (x-1, t-1))

            r = alpha * (?dt / (?dx * ?dx))

         in arrayMemoFix ((0, 0), (?nx, ?nt)) h
 
experiment = let ?dx = 0.1 in
             let ?dt = 0.1 in
             let ?nx = 6  in
             let ?nt = 10  in
             let alpha = 0.05 
                 f = check (spec (impl alpha) (Constant alpha))
                 outputFun (x, t) = putStrLn $ "x = " ++ (show x) ++ " t = " ++ (show t)
                                               ++ " results = " ++ (show $ f (x,t))
             in mapM_ outputFun [(0,0)..(?nx-2,?nt-1)]




