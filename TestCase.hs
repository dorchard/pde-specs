{-# LANGUAGE ImplicitParams, TemplateHaskell, TypeOperators #-}

import Data.Function.ArrayMemoize
import Debug.Trace

import Plot
import PDESpec

spec alpha h = ((d h T) === (alpha * d2 h X))  `withDomain`   (X :. T :. Nil)

impl alpha = 
         let

           h :: Unfix ((Int, Int) -> Float)
           h h' (x, t)
               | x == 0    = 1
               | x == ?nx  = h' (x - 1, t)
               | t == 0    = 0
               | otherwise = h' (x, t-1) + r * (h' (x+1, t-1) - 2 * h' (x, t-1) + h' (x-1, t-1))
           r = alpha * (?dt / (?dx * ?dx))

         in arrayMemoFix ((0, 0), (?nx, ?nt)) h

 
experiment = let ?dx = 0.05 in
             let ?dt = 0.05 in
             let ?nx = 40  in
             let ?nt = 500  in
             let alpha = 0.006
                 f = check (spec (Constant alpha) (impl alpha))
                 outputFun (x, t) = putStrLn $ "x = " ++ (show x) ++ " t = " ++ (show t)
                                               ++ " results = " ++ (show $ f (x,t))

                 figure = plot3d' 1 1 (0, ?nx) (0, ?nt) "x" "t" "heat" (curry (impl alpha))

                                                  
             in do mapM_ outputFun [(0,0)..(?nx-2,?nt-1)]
                   plotX11 figure

model_obj :: Model (X :. T :. Nil) ((Int, Int) -> Float)
model_obj = 
             let ?dx = 0.05 in
             let ?dt = 0.05 in
             let ?nx = 40  in
             let ?nt = 500  in
             let alpha = 0.006
                 m = Model (spec (Constant alpha)) (impl alpha)
             in m
             
    
