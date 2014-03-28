{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, ImplicitParams #-}

module Output (module Text.LaTeX.Packages.AMSMath, module Output) where

{- (<>), fromString, LaTeX(..),alpha,deltau,fixLatex,fixLatexCases,doLatex,writeLatexFigure,mathit) where -}

import Text.LaTeX.Base hiding (alph)
import Text.LaTeX.Packages.AMSMath 
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Base.Render

import Data.Text hiding (map)
import Types


class Output t where
    toLatex :: (?name :: String) => t -> LaTeX
    toString :: (?name :: String) => t -> String

instance (Show a, Fractional a) => Output (Spec ds (t -> a)) where
    toLatex (Equality e1 e2 _) = (toLatex e2) `equiv` (toLatex e2)
    toString (Equality e1 e2 _) = toString e1 ++ "=" ++ toString e2

instance (Show a, Fractional a) => Output (Eqn ds (t -> a)) where
    toLatex (Delta n _ d) = ((delta ^: (fromInteger n)) <> (fromString ?name)) 
                           `frac` 
                           ((delta <> (fromString $ show d)) ^: (fromInteger n))
    toLatex (Times e1 e2) = (parensL $ toLatex e1) * (parensL $ toLatex e2)
    toLatex (Add e1 e2) = (toLatex e1) + (toLatex e2)
    toLatex (Add e1 e2) = (toLatex e1) - (toLatex e2)
    toLatex (Divide e1 e2) = (toLatex e1) `frac` (toLatex e2)
    toLatex (Abs e1) = (fromString "|") <> (toLatex e1) <> (fromString "|")
    toLatex (Constant e1) = (fromString $ show e1)

    toString (Delta 1 _ d)  = "d " ++ ?name ++ " / " ++ "d " ++ (show d)
    toString (Delta n _ d)  = "d^" ++ (show n) ++ " " ++ ?name ++ " / " 
                           ++ "d " ++ (show d) ++ "^" ++ (show n)
    toString (Times e1 e2)  = toString e1 ++ "*(" ++ toString e2 ++ ")"
    toString (Add e1 e2)    = toString e1 ++ "+" ++ toString e2
    toString (Minus e1 e2)  = toString e1 ++ "-" ++ toString e2
    toString (Divide e1 e2) = toString e1 ++ " / " ++ toString e2
    toString (Abs e1)       = "|" ++ toString e1 ++ "|"
    toString (Constant e1)  = show e1

juxtL t1 t2 = t1 <> (fromString " ") <> t2
parensL t = (fromString "(") <> t <> (fromString ")")


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