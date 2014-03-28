import Plot
import Data.Function.ArrayMemoize

-- d h / d t = a * (d^2 h / d x^2) 

import Debug.Trace

data Dimensions = X | Y | Z | T deriving Show

data Spec a = Delta Int Dimensions | Equality (Spec a) (Spec a) | Times (Spec a) (Spec a) | Constant Float
               deriving Show


instance Num (Spec a) where
    a * b = Times a b

instance Fractional (Spec a) where
    fromRational x = Constant (fromRational x)

(===) = Equality

d = Delta 1
d2 = Delta 2

-- h :: Float -> Float -> Float 

type Fix f = f -> f

spec a = (d T) === ((Constant a) * (d2 X))

impl dx dt nx nt a = 
         let
            h :: Fix ((Int, Int) -> Float)
            h h' (x, t)
              | x == 0  = 1
              | x == nx = h' (x - 1, t)
              | t == 0  = 0
              | otherwise =  h' (x, t-1) + r * (h' (x+1, t-1) - 2 * h' (x, t-1) + h' (x-1, t-1))
            r = a * (dt / (dx*dx))
         in ("r = " ++ show r) `trace` arrayMemoFix ((0, 0), (nx, nt)) h

implA dx dt nx nt a = impl dx dt nx nt a

{-
test = check nx nt (spec undefined) (implA undefined) 
            where a = 0.05
                  nx = 5
                  nt = 5
-}

check' dx dt  (Delta 1 dim) impl = euler dx dt dim impl 
check' dx dt  (Delta 2 dim) impl = (euler dx dt dim) . (euler dx dt dim) $ impl 
check' dx dt  (Times (Constant a) s) impl = \(x,t) -> a * impl (x,t)

check dx dt  (Equality s1 s2) impl = \(x,t) -> (check' dx dt s1 impl (x,t), check' dx dt s2 impl (x,t))

euler dx dt d h = localEuler dx dt d h

localEuler dx dt X h (x, t) = (h (x + 1, t) - h (x, t)) / dx
localEuler dx dt T h (x, t) = (h (x, t + 1) - h (x, t)) / dt
                   
experiment = let comparisonFun = check dx dt (spec a) (implA dx dt nx nt a) 
                 a = 0.05
                 dx = 0.1
                 dt = 0.1
                 nx = 5
                 nt = 10
             in mapM_ (\(x,t) -> putStrLn $ "x = " ++ (show x) ++ " t = " ++ (show t) ++ 
                                               " results = " ++ (show $ comparisonFun (x,t))) [(0,0)..(nx-1,nt-1)]





