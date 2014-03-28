{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs #-}

module LaTeX (module Text.LaTeX.Packages.AMSMath, module LaTeX) where

{- (<>), fromString, LaTeX(..),alpha,deltau,fixLatex,fixLatexCases,doLatex,writeLatexFigure,mathit) where -}

import Text.LaTeX.Base hiding (alph)
import Text.LaTeX.Packages.AMSMath 
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Base.Render

import Data.Text hiding (map)
import Types

specToLatex (Equality e1 e2 _) = (eqnToLatex e2) `equiv` (eqnToLatex e2)

eqnToLatex (Delta n _ d) = undefined


{- Some stuff missing from LaTeX package -}

instance Ord LaTeX where

instance Real LaTeX where
    
instance RealFrac LaTeX where


{- LaTeX fixed point #-}
    
fixLatex :: String -> (((LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX
fixLatex recName f = \(x, y) -> ((fromString recName) ^: x !: y) `equiv`  (f (\(x, y) -> (fromString recName) ^: x !: y) (x, y))

fixLatexD :: String -> (LaTeX -> LaTeX -> LaTeX) -> (((LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX) -> (LaTeX, LaTeX) -> LaTeX
fixLatexD recName delim f = \(x, y) -> delim ((fromString recName) ^: x !: y) (fromString "=" <> (f (\(x, y) -> (fromString recName) ^: x !: y) (x, y)))

fixLatexCases recName f cases = align $ map (fixLatexD recName (&) f) cases

doLatex eqn name = renderFile (name ++ ".tex") (wrapped eqn)
                    where wrapped eqn = (documentclass [] article) <> usepackage [] "amsmath" <> (document eqn) -- (equiv (mathit $ fromString name) eqn)))

writeLatexFigure eqn figname name = 
       renderFile name ((documentclass [] article) 
                                     <> usepackage [] "amsmath" 
                                     <> usepackage [] "graphicx"
                                    <> (document (eqn <> (includegraphics [] figname))))