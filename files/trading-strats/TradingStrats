{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
--  _____               _ _               __ _             _       
-- /__   \_ __ __ _  __| (_)_ __   __ _  / _\ |_ _ __ __ _| |_ ___ 
--   / /\/ '__/ _` |/ _` | | '_ \ / _` | \ \| __| '__/ _` | __/ __|
--  / /  | | | (_| | (_| | | | | | (_| | _\ \ |_| | | (_| | |_\__ \
--  \/   |_|  \__,_|\__,_|_|_| |_|\__, | \__/\__|_|  \__,_|\__|___/
--                                |___/                   
module TradingStrats () where

import qualified Data.Map.Strict   as Map
import           Data.Series       ( Series )
import qualified Data.Series       as Series
import           Data.Time         ( UTCTime )
import           Pipes             ( Pipe, Producer, (>->) )
import qualified Pipes
import qualified Pipes.Prelude     as Pipes         

type Price = Double

data Action = Buy Int | Sell | Hold

newtype Strategy r
    = MkStrategy { runStrategy :: r -> Action }


backtestStrategy :: (Feature r, Monad m) 
                 => Strategy r  -- ^ Strategy
                 -> Parameters r  -- ^ Feature set parameters
                 -> Producer (UTCTime, Price)  m ()
                 -> Producer (UTCTime, Action) m ()
backtestStrategy strat params prices =
    deriveFeature params prices 
        >-> Pipes.map (\(k, feature) -> (k, runStrategy strat feature)) 
    

class Feature r where
    -- | Features can be parametrized. For example, a 
    -- feature such as the rolling average price is parametrized
    -- by a rolling window length. The 'Parameters' type family
    -- allows to specify what type is used to parametrize a feature.
    type Parameters r


    deriveFeature :: Monad m 
                  => Parameters r
                  -> Producer (UTCTime, Price) m ()
                  -> Producer (UTCTime, r) m ()



instance (Feature a, Feature b) => Feature (a, b) where
    type Parameters (a, b) = (Parameters a, Parameters b)
    deriveFeature :: Monad m 
                  => Parameters (a, b)
                  -> Producer (UTCTime, Price) m ()
                  -> Producer (UTCTime, (a, b)) m ()
    deriveFeature (paramsA, paramsB) prices 
        = Pipes.zipWith (\(k,a) (_, b) -> (k, (a, b))) 
                        (deriveFeature paramsA prices) 
                        (deriveFeature paramsB prices)


newtype PriceHistory = MkPriceHistory (Series UTCTime Price)

newtype NumTicks = MkNumTicks { numTicks :: Int }

instance Feature PriceHistory where
    type Parameters PriceHistory = NumTicks

    deriveFeature :: Monad m 
                  => NumTicks
                  -> Producer (UTCTime, Price) m ()
                  -> Producer (UTCTime, PriceHistory) m ()
    deriveFeature (MkNumTicks numTicks) prices
        = prices >-> accumulate numTicks >-> Pipes.map (\xs -> (maximum $ Series.index xs, MkPriceHistory xs))


accumulate :: Functor m 
           => Int 
           -> Pipe (UTCTime, a) (Series UTCTime a) m () 
accumulate windowLength = go mempty
    where
        go accumulator = do
            (key, val) <- Pipes.await
            let newAccumulator = Map.insert key val accumulator 
            if Map.size accumulator == windowLength
                then do
                    Pipes.yield $ Series.fromStrictMap newAccumulator
                    go newAccumulator
                else do
                    let (_, newAccumulator') = Map.deleteFindMin newAccumulator
                    Pipes.yield $ Series.fromStrictMap newAccumulator'
                    go newAccumulator'


finalStrategy :: Strategy (PriceHistory, Price)
finalStrategy 
    = MkStrategy $ \(MkPriceHistory history, price) 
        -> let avgPrice = Series.fold Series.mean history
            in case price `compare` avgPrice of
                GT -> Sell
                LT -> Buy 10
                EQ -> Hold 