{-# LANGUAGE ImplicitParams, TemplateHaskell, TypeOperators, ConstraintKinds, NoMonomorphismRestriction #-}

import Data.Function.ArrayMemoize
import Debug.Trace

import Plot
import PDESpec

spec alpha h = ((d h T) === (alpha * d2 h X))  `withDomain`   (X :. T :. Nil)

impl alpha h' (x, t)
    | x == 0    = 1
    | x == ?nx  = h' (x - 1, t)
    | t == 0    = 0
    | otherwise = h' (x, t-1) + r * (h' (x+1, t-1) - 2 * h' (x, t-1) + h' (x-1, t-1))
                   where r = alpha * (?dt / (?dx * ?dx))

implFast alpha = arrayMemoFix ((0 :: Int, 0 :: Int), (?nx, ?nt)) (impl alpha)


experiment = let ?dx = 0.05 in
             let ?dt = 0.05 :: Float in
             let ?nx = 40 :: Int in
             let ?nt = 20 :: Int  in
             let ?name = "h" in 
             let alpha = 0.006 
                 spec' = spec (constant alpha) (implFast alpha)
                 f     = check spec'

                 outputFun (x, t) = putStrLn $ "x = " ++ (show x) ++ " t = " ++ (show t)
                                    ++ " results = " ++ (show $ f (x,t))
                 figure = plot3d' 1 1 (0, ?nx) (0, ?nt) (show X) (show T) (?name) (curry (implFast alpha))
                 figureEqn axis xs = plot3d' 1 1 (0, ?nx - 2) (0, ?nt - 1) (show X) (show T) axis xs

                                                  
             in do dat <- mapM outputFun [(0,0)..(?nx-2,?nt-1)]
                   doLatex  (toLatex (spec (varconst "alpha") (implFast alpha))) ?name
                   --putStrLn $ (toString $ lhs spec')
                   --putStrLn $ (toString $ rhs spec')
                   --plotX11 figure
                   --plotX11 (figureEqn (toString $ lhs spec') (curry $ fst . f)) 
                   --plotX11 (figureEqn (toString $ rhs spec') (curry $ snd . f)) 
                   --plotX11 (figureEqn "|err|" (\x t -> (abs . uncurry (-)) . f $ (x, t) )) 
                   return ()


experimentCSV fname = let ?dx = 0.05 in
                      let ?dt = 0.05 :: Float in
                      let ?nx = 20 :: Int in
                      let ?nt = 50 :: Int in
                      let alpha = 0.006
                          f = check (spec (constant alpha) (implFast alpha))
                          outputRow (x, t) = [show x, show t, show . fst $ f (x, t), show . snd $ f (x, t)]
                          csv = map outputRow [(0,0)..(?nx-2, ?nt-1)]

                      in writeFile fname (printCSV csv)


model_obj :: Model (X :. T :. Nil) ((Int, Int) -> Float)
model_obj = 
             let ?dx = 0.05 in
             let ?dt = 0.05 in
             let ?nx = 40  :: Int in
             let ?nt = 500 :: Int in
             let alpha = 0.006
                 m = Model (spec (Constant alpha Nothing)) (implFast alpha)
             in m
             
