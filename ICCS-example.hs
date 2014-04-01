{-# LANGUAGE ImplicitParams #-}
import PDESpec

{- Specification of heat equation PDE -}
spec h = (d h T) === (constant ?alpha * d2 h X)  `withDomain`   (X :. T :. Nil)

{- Implementation using a recurrence relation -}
approx h' (x, t)
    | x == 0    = 1
    | x == ?nx  = h' (x - 1, t)
    | t == 0    = 0
    | otherwise = h' (x, t-1) + r * (h' (x+1, t-1) - 2 * h' (x, t-1) + h' (x-1, t-1))
                   where r = ?alpha * (?dt / (?dx * ?dx))

{- Create a fast version using a memoizing combinators -}
approxFast :: (?nx :: Int, ?nt :: Int, ?dx :: Float, ?dt :: Float, ?alpha :: Float) =>
            (Int, Int) -> Float
approxFast = (arrayMemoFix ((0, 0), (?nx, ?nt)) approx)

{- Instantiate and calculate absolute error of 'approxFast' based on the spec -}
experiment = let ?dx = 0.05 
                 ?dt = 0.05 
                 ?nx = 100 
                 ?nt = 100
                 ?alpha = 0.006
             in verifyModel Euler spec approxFast

