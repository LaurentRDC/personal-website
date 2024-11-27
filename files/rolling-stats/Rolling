-- ######                                                                           
-- #     #  ####  #      #      # #    #  ####      ####  #####   ##   #####  ####  
-- #     # #    # #      #      # ##   # #    #    #        #    #  #    #   #      
-- ######  #    # #      #      # # #  # #          ####    #   #    #   #    ####  
-- #   #   #    # #      #      # #  # # #  ###         #   #   ######   #        # 
-- #    #  #    # #      #      # #   ## #    #    #    #   #   #    #   #   #    # 
-- #     #  ####  ###### ###### # #    #  ####      ####    #   #    #   #    ####  
--
-- Tested with GHC 9.4.4, vector 0.13.0.0, and statistics 0.16.1.2

module Rolling ( naiveRolling, rollingMean, rollingVar, rollingSharpe ) where

import           Data.Vector        ( Vector )   
import qualified Data.Vector        as Vector
import           Statistics.Sample  ( mean, varianceUnbiased )


-- | Generic rolling function.
-- This implementation has complexity O ( N * M ), where N is the length
-- of the input and M is the window length.
naiveRolling :: Int                        -- ^ Window length
             -> (Vector Double -> Double)  -- ^ Aggregation function, e.g. the mean
             -> Vector Double              -- ^ Input series
             -> Vector Double
naiveRolling window f vs
    = let go ix 
            -- If the index is too small to fit an entire window, fill with 0
            -- and move on to the next index
            | ix - window + 1 < 0 = (0, ix+1)
            -- If the index is large enough to fit an entire window, calculate
            -- the value at index `ix` to be `f` on the appropriate slice
            -- and move on to the next index
            | otherwise           = (f $ Vector.slice (ix - window + 1) window vs, ix + 1)
       in Vector.unfoldrExactN (Vector.length vs) go 0


-- | Perform the rolling mean calculation on a vector.
--
-- >>> :set -XOverloadedLists
-- >>> rollingMean 2 [0,1,2,3,4,5]
-- [0.0,1.5,2.5,3.5,4.5]
rollingMean :: Int            -- ^ Window length
            -> Vector Double  -- ^ Input series
            -> Vector Double  
rollingMean window vs
    = let -- Starting point is the mean of the first complete window
          start = mean (Vector.take window vs)
          
          -- Consider the recurrence relation mean[i] = mean[i-1] + (edge - lag)/w 
          -- where w    = window length
          --       edge = vs[i]
          --       lag  = vs[i - w]
          edge = Vector.drop window vs
          lag  = Vector.take (Vector.length vs - window) vs
          diff = Vector.zipWith (\p n -> (p - n) / fromIntegral window) edge lag
      
    -- The rolling mean for the elements at indices i < window is set to 0
       in Vector.replicate (window - 1) 0 <> Vector.scanl (+) start diff


-- | Perform the rolling unbiased variance calculation on a vector.
--
-- >>> :set -XOverloadedLists
-- >>> rollingVar 2 [1,2,3,4,3,2,1]
-- [0.0,0.5,0.5,0.5,0.5,0.5,0.5]
rollingVar :: Int          
           -> Vector Double
           -> Vector Double
rollingVar window vs
    = let start   = varianceUnbiased $ Vector.take window vs
          n       = fromIntegral window
          ms      = rollingMean window vs
        
          -- Rolling mean terms leading by N
          ms_edge = Vector.drop window ms
          -- Rolling mean terms leading by N - 1
          ms_lag  = Vector.drop (window - 1) ms
          
          -- Values leading by N
          xs_edge = Vector.drop window vs
          -- Values leading by 0
          xs_lag  = vs
          
          -- Implementation of the recurrence relation, minus the previous term in the series
          -- There's no way to make the following look nice, sorry.
          -- N * \bar{x}^2_{N-1} - N * \bar{x}^2_{N} + x^2_N - x^2_0
          term xbar_nm1 xbar_n x_n x_0 = (n * (xbar_nm1**2) - n * (xbar_n ** 2) + x_n**2 - x_0**2)/(n - 1)
        
    -- The rolling variance for the elements at indices i < window is set to 0
       in Vector.replicate (window - 1) 0 <> Vector.scanl (+) start (Vector.zipWith4 term ms_lag ms_edge xs_edge xs_lag)


-- | Perform the rolling Sharpe ratio of a vector of returns.
rollingSharpe :: Int          
              -> Vector Double
              -> Vector Double
rollingSharpe window vs
    = let start   = varianceUnbiased $ Vector.take window vs
          n       = fromIntegral window
          ms      = rollingMean window vs
          
          -- The following expressions are taken from rollingVar
          ms_edge = Vector.drop window ms
          ms_lag  = Vector.drop (window - 1) ms
          xs_edge = Vector.drop window vs
          xs_lag  = vs
          term xbar_nm1 xbar_n x_n x_0 = (n * (xbar_nm1**2) - n * (xbar_n ** 2) + x_n**2 - x_0**2)/(n - 1)

          -- standard deviation from variance
          std = sqrt <$> Vector.scanl (+) start (Vector.zipWith4 term ms_lag ms_edge xs_edge xs_lag)
        
    -- The rolling Sharpe ratio for the elements at indices i < window is set to 0
       in Vector.replicate (window - 1) 0 <> Vector.zipWith (/) (Vector.drop window ms) std