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

-- Make it fast please Mrs. Haskell:
-- implFast :: Float -> (Int, Int) -> Float
implFast alpha = arrayMemoFix ((0 :: Int, 0 :: Int), (?nx, ?nt)) (impl alpha)

-- Build experiment with various choices of output

experiment action = 
             let ?dx = 0.05 
                 ?dt = 0.05 :: Float
                 ?nx = 100   :: Int
                 ?nt = 100   :: Int
                 ?name = "h" in 

             let alpha = 0.006 
                 spec' = spec (constant alpha)

                 results = verifyModel Euler spec' (implFast alpha)

                 figureEqn axis xs = plot3d' 1 1 (0, ?nx - 2) (0, ?nt - 1) (show X) (show T) axis xs

                 heatImpl = makePlot "h" (implFast alpha) 
                 heatLHS = makePlotAdjust 2 1 (toString $ lhs $ spec' (implFast alpha)) (fst . results)
                 heatRHS = makePlotAdjust 2 1 (toString $ rhs $ spec' (implFast alpha)) (snd . results)
                 heatErr = makePlotAdjust 2 1 "|err|" (abs . uncurry (-) . results)
         
             in case action of
                  ShowFigures -> showFigures [heatImpl, heatLHS, heatRHS, heatErr]
                  PNGFigures  -> writePNGs [(heatImpl, "heatImpl"), 
                                            (heatLHS, "heatLHS"),
                                            (heatRHS, "heatRHS"),
                                            (heatErr, "heatErr")]
                  Latex       -> outputLatexSep --(impl alpha) -- Docs spec' impl 
                  CSV         -> let outputRow (x, t) = let (lhs, rhs) = results (x, t)
                                                        in [show x, show t, show (implFast alpha (x, t)), 
                                                            show lhs, show rhs]
                                     csv = map outputRow [(0,0)..(?nx-2, ?nt-1)]
                                 in writeFile "data" (printCSV csv)


outputLatexSep = 
                 let implTex =
                               let ?nx = mathit $ fromString "nx"
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
             