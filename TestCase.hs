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
             let ?nt = 20  in
             let ?name = "h" in 
             let alpha = 0.006
                 spec' = spec (Constant alpha) (impl alpha)
                 f = check spec'
                 outputFun (x, t) = putStrLn $ "x = " ++ (show x) ++ " t = " ++ (show t)
                                               ++ " results = " ++ (show $ f (x,t))

                 figure = plot3d' 1 1 (0, ?nx) (0, ?nt) (show X) (show T) (?name) (curry (impl alpha))
                 figureEqn axis xs = plot3d' 1 1 (0, ?nx - 2) (0, ?nt - 1) (show X) (show T) axis xs

                                                  
             in do --dat <- mapM outputFun [(0,0)..(?nx-2,?nt-1)]
                   doLatex  (toLatex spec') ?name
                   putStrLn $ (toString $ lhs spec')
                   putStrLn $ (toString $ rhs spec')
                   --plotX11 figure
                   --plotX11 (figureEqn (toString $ lhs spec') (curry $ fst . f)) 
                   --plotX11 (figureEqn (toString $ rhs spec') (curry $ snd . f)) 
                   --plotX11 (figureEqn "|err|" (\x t -> (abs . uncurry (-)) . f $ (x, t) )) 

experimentCSV fname = let ?dx = 0.05 in
                      let ?dt = 0.05 in
                      let ?nx = 20  in
                      let ?nt = 50  in
                      let alpha = 0.006
                          f = check (spec (Constant alpha) (impl alpha))
                          outputRow (x, t) = [show x, show t, show . fst $ f (x, t), show . snd $ f (x, t)]
                          csv = map outputRow [(0,0)..(?nx-2, ?nt-1)]

                      in writeFile fname (printCSV csv)

model_obj :: Model (X :. T :. Nil) ((Int, Int) -> Float)
model_obj = 
             let ?dx = 0.05 in
             let ?dt = 0.05 in
             let ?nx = 40  in
             let ?nt = 500  in
             let alpha = 0.006
                 m = Model (spec (Constant alpha)) (impl alpha)
             in m
             
    
