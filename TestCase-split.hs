{-# LANGUAGE ImplicitParams, TemplateHaskell, TypeOperators, ConstraintKinds, NoMonomorphismRestriction #-}

import PDESpec

-- Example specification 

spec alpha h = ((d h T) === (alpha * d2 h X))  `withDomain`   (X :. T :. Nil)

-- Example FCTS implementation

impl alpha h' (x, t)
    | x == 0    = 1
    | x == ?nx  = h' (x - 1, t)
    | t == 0    = 0
    | otherwise = h' (x, t-1) + r * (h' (x+1, t-1) - 2 * h' (x, t-1) + h' (x-1, t-1))
                   where r = alpha * (?dt / (?dx * ?dx))

-- Make it fast please Mr. Haskell:
implFast alpha = arrayMemoFix ((0 :: Int, 0 :: Int), (?nx, ?nt)) (impl alpha)

outputLatex = let implTex = let ?nx = mathit $ fromString "nx"
                                ?dx = deltau <> (fromString "x")
                                ?dt = deltau <> (fromString "t") 
                            in fixLatexCases "h" (impl alpha)
                                   [(0, fromString "t"), 
                                    (?nx, fromString "t"),
                                    (fromString "x", 0),
                                    (fromString "x", fromString "t")]
                  specTex = let ?name = "h"
                                ?dx = undefined
                                ?dt = undefined
                            in toLatex (spec (varconst "alpha") (undefined::(Int, Int) -> Float))
              in doLatex (noindent <> fromString "Abstract specification : " <>
                           equation specTex                    <> 
                          fromString "Discrete approximation : " <>
                           implTex) "heat"


experiment = let ?dx = 0.05 
                 ?dt = 0.05 :: Float
                 ?nx = 40   :: Int
                 ?nt = 20   :: Int
                 ?name = "h" in 

             let alpha = 0.006 
                 spec' = let ?nx = ?nx :: Int
                             ?nt = ?nt :: Int
                         in spec (constant alpha) (implFast alpha)
                 f     = check spec'

                 outputFun (x, t) = putStrLn $ "x = " ++ (show x) ++ " t = " ++ (show t)
                                    ++ " results = " ++ (show $ f (x,t))
                 figureEqn axis xs = plot3d' 1 1 (0, ?nx - 2) (0, ?nt - 1) (show X) (show T) axis xs

                 dat = map outputFun [(0,0)..(?nx-2,?nt-1)]

                 heatImpl = plot3d' 1 1 (0, ?nx) (0, ?nt) (show X) (show T) (?name) (curry (implFast alpha))
                 heatLHS = figureEqn (toString $ lhs spec') (curry $ fst . f)
                 heatRHS = figureEqn (toString $ rhs spec') (curry $ snd . f)
                 heatErr = figureEqn "|err|" (\x t -> (abs . uncurry (-)) . f $ (x, t))
                                                  
             in do plotX11 heatImpl
                   plotX11 heatLHS
                   plotX11 heatRHS
                   plotX11 heatErr
                   
                   writePlotPNG heatImpl "heatImpl"
                   writePlotPNG heatLHS  "heatLHS"
                   writePlotPNG heatRHS  "heatRHS"
                   writePlotPNG heatErr  "heatErr"

experimentCSV fname = let ?dx = 0.05 in
                      let ?dt = 0.05 :: Float in
                      let ?nx = 20 :: Int in
                      let ?nt = 50 :: Int in
                      let alpha = 0.006
                          f = check (spec (constant alpha) (implFast alpha))
                          outputRow (x, t) = [show x, show t, show . fst $ f (x, t), show . snd $ f (x, t)]
                          csv = map outputRow [(0,0)..(?nx-2, ?nt-1)]

                      in writeFile fname (printCSV csv)



main = do args <- getArgs
          let ?dx = read $ args !! 0 :: Float
              ?dt = read $ args !! 1 :: Float
              ?nx = read $ args !! 2 :: Int
              ?nt = read $ args !! 3 :: Int
              ?name = "h"
           in
             let alpha = read $ args !! 4 :: Float

                 spec' = spec (constant alpha) (implFast alpha)
                 f     = check spec'

                 figureEqn axis xs = plot3d' 1 1 (0, ?nx - 2) (0, ?nt - 1) (show X) (show T) axis xs

                 heatImpl = plot3d' 1 1 (0, ?nx) (0, ?nt) (show X) (show T) (?name) (curry (implFast alpha))
                 heatLHS = figureEqn (toString $ lhs spec') (curry $ fst . f)
                 heatRHS = figureEqn (toString $ rhs spec') (curry $ snd . f)
                 heatErr = figureEqn "|err|" (\x t -> (abs . uncurry (-)) . f $ (x, t))
                 
                 outputRow (x, t) = [show x, show t, show . fst $ f (x, t), show . snd $ f (x, t)]
                 csv = map outputRow [(0,0)..(?nx-2, ?nt-1)]
                               
             in do putStrLn "data.csv"
                   writeFile "data.csv" (printCSV csv)
                   putStrLn "heatImpl.png"
                   writePlotPNG heatImpl "heatImpl"
                   putStrLn "heatLHS.png"
                   writePlotPNG heatLHS  "heatLHS"
                   putStrLn "heatRHS.png"
                   writePlotPNG heatRHS  "heatRHS"
                   putStrLn "heatErr.png"
                   writePlotPNG heatErr  "heatErr"
                   outputLatexSep

outputLatexSep = let implTex = let ?nx = mathit $ fromString "nx"
                                   ?dx = deltau <> (fromString "x")
                                   ?dt = deltau <> (fromString "t") 
                               in fixLatexCases "h" (impl alpha)
                                   [(0, fromString "t"), 
                                    (?nx, fromString "t"),
                                    (fromString "x", 0),
                                    (fromString "x", fromString "t")]
                     specTex = let ?name = "h"
                                   ?dx = undefined
                                   ?dt = undefined
                               in toLatex (spec (varconst "alpha") (undefined::(Int, Int) -> Float))
                 in do putStrLn "model.tex"
                       writeFile "model.tex" (unpack . render $ specTex)
                       putStrLn "impl.tex"
                       writeFile "impl.tex" (unpack . render $ implTex)
                       putStrLn "heat.tex"
                       doLatex (noindent <> fromString "Abstract specification : " <>
                           equation specTex                    <> 
                          fromString "Discrete approximation : " <>
                           implTex) "heat"
            
             