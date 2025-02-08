---
title: Trading strategies with typed features using Haskell and type families
date: 2024-02-04
summary: In this blog post, I show you the basics of defining typed features in trading strategies using Haskell.
tags: haskell, finance
---

I work in the business of algorithmic power trading, which is the automated trading of various power-related products in regulated [electricity markets](https://en.wikipedia.org/wiki/Electricity_market). Products include short-term inter-jurisdiction arbitrage, financial transmission rights, and more. 

This year, my employer is expanding its trading operations to a new class of products. Since there is no overlap between this new work and our current operations, I got to design a technology stack most suited for the task. This technology stack includes Haskell, most importantly because wrong or unexpected trading decisions can (and have) cost us dearly.

In this blog post, I want to show you the basics of how we designed the framework in which to express trading strategies. 

## The fundamentals of trading strategies

The fundamental pieces of trading operations are *strategies*. In algorithmic trading, strategies are computer programs that decide what to trade, and how to trade it, at any given moment. 

Let's take the example of a simple trading strategy that is only concerned with [AAPL stock](https://www.nasdaq.com/market-activity/stocks/aapl). The current stock price is about 190 USD today; our example strategy is defined thusly:

* If the AAPL price rises above 200, sell our holdings (if we have any);
* If the AAPL price falls below 180, buy 10 shares;

In this case, the result of this strategy is some signal to buy or sell AAPL stock. We can run our strategy in a loop:

```haskell
import qualified Control.Monad

data Action = Buy Int 
            | Sell 
            | Hold

type Price = Double

main :: IO ()
main = Control.Monad.forever $ do
    aaplPrice <- fetchMostRecentPrice
    let action = myStrategy aaplPrice
    executeMarketAction action
    where
        myStrategy :: Price -> Action
        myStrategy aapl_price
            | aapl_price > 200 = Sell
            | aapl_price < 180 = Buy 10
            | otherwise        = Hold

        fetchMostRecentPrice :: IO Price
        fetchMostRecentPrice = (...)

        executeMarketAction :: Action -> IO ()
        execureMarketAction = (...)
```

and boom, you have a simple trading system!

Once you have a good idea for a strategy, you should test it on historical data. This is called *backtesting*. Backtesting strategies is, by definition, much more computationally intensive than live trading, since you are evaluating your strategy on much more data. We often backtest strategies on 5-10 years' worth of data when it makes sense, and sometimes more. 

I will also note that it is easiest to have strategies that can run in various contexts (including backtesting and live operations) if the strategy is *pure* (in the [mathematical sense](https://en.wikipedia.org/w/index.php?title=Pure_function&oldid=1192610707)). It is in the quest for purity and performance that we decided to implement the trading system for a new asset class in Haskell.

## Trading strategies in Haskell

For the simplicity of presentation, we will only consider strategies that involve prices. The simplest such strategies are strategies which depend on the *most recent* price:

```haskell
newtype Strategy
    = MkStrategy { runStrategy :: Price -> Action }
```

`Strategy` is a type of functions, from the most recent `Price` known to some market action. This is only re-packaging the example above.

Let's build a backtesting framework. There are two parts here:

* determine historical market actions;
* simulate the effects of market actions.

In practice, the two parts of backtesting are handled simultaneously. However, for simplicity, I will only consider the first part here.

The nature of this problem is well-suited to streaming approaches; I will use [`pipes`](https://hackage.haskell.org/package/pipes)[^pipes]:

[^pipes]: If you are unfamiliar with `pipes`, you should check out its [tutorial](https://hackage.haskell.org/package/pipes-4.3.16/docs/Pipes-Tutorial.html).

```haskell
-- From the `time` package
import           Data.Time     ( UTCTime )
-- From the `pipes` package
import           Pipes         ( Producer, (>->) )
import qualified Pipes
import qualified Pipes.Prelude as Pipes

-- | From a stream of input features, produce a stream
-- of output 'Market' actions 
backtestStrategy :: Monad m 
                 => Strategy r
                 -> Producer (UTCTime, Price)  m () -- ^ stream of timestamped AAPL prices
                 -> m BacktestResults
backtestStrategy strat prices 
    =   prices 
    >-> Pipes.map (\(k, f) -> (k, runStrategy strat f)) 
    >-> simulateMarketActions

-- The following is out-of-scope

data BacktestResults = MkBacktestResults (...)

simulateMarketActions :: Consumer (UTCTime, Action) m BacktestResults
simulateMarketActions = (...)
```

## More expressive strategies

I have a problem with the above definition of `Strategy`: I'm limited to strategies based on the single, most recent `Price`. What if I had a good idea for a strategy which involves the last 10 price values? Our `Strategy` type is not expressive enough: it only takes one type of *feature*, while we want to support a wide range of features.

I will define a `Strategy` type which removes restrictions on the input feature:

```haskell
newtype Strategy r 
    = MkStrategy { runStrategy :: r -> Action }
```

with the understanding that the data of type `r` is somehow *derived* from prices. 

What are some features derived from prices, that we might be interested in?

* Price history, e.g. most recent N ticks;
* Price aggregations, e.g. average of most recent M ticks;
* [Rolling aggregations](/posts/rolling-stats.html), e.g. N-tick history of the averages of M ticks;

Every conceptual feature described above has some free parameters. We don't want to have separate strategies like `Strategy PriceHistoryForPast10Ticks` and `Strategy PriceHistoryForPast20Ticks`. More specifically, for every feature of type `r`, there is a type of parameters `p` which describes the parameters of `r`.

For example[^javelin]:

```haskell
-- from the `javelin` package
import Data.Series ( Series )

-- Feature I may want to use in a strategy
newtype PriceHistory 
    = MkPriceHistory (Series UTCTime Price)

-- Parameters that may affect the featyre `PriceHistory`
data NumTicks 
    = MkNumTicks { numTicks :: Int }
```

[^javelin]: We are storing the price history in a `Series`, which comes from the [`javelin` package](hackage.haskell.org/package/javelin) that I created specifically for this work.

## Typed features and their parametrization

We could list all possible features in a big sum type:

```haskell
data Feature 
    = FPrice Price
    | FPriceHistory (Series UTCTime Price)
    | FAveragePrice Price
    | (...)
```

However, it's rather cumbersome to create a strategy to experiment with new features. We can do better.

We want to be able to link the types `PriceHistory` and `NumTicks` such that they are used together when backtesting, to ensure type safety. This is the domain of [indexed type families](https://wiki.haskell.org/GHC/Type_families), or type families for short. We will amend our `Feature` type to become a typeclass, and upgrade `backtestStrategy` function to take into account feature parametrization:

```haskell
class Feature r where
    -- For every instance `r` of `Feature`,
    -- there is an associated type `Parameters r` which the user
    -- needs to specify. See examples below.
    type Parameters r

    deriveFeature :: Monad m 
                  => Parameters r
                  -> Producer (UTCTime, Price) m ()
                  -> Producer (UTCTime, r) m ()

backtestStrategy :: (Feature r, Monad m) 
                 => Strategy r
                 -> Parameters r
                 -> Producer (UTCTime, Price)  m ()
                 -> m BacktestResults
backtestStrategy strat params prices 
    =   deriveFeature params prices 
    >-> Pipes.map (\(k, feature) -> (k, runStrategy strat feature)) 
    >-> simulateMarketActions
```

Let's look at two example instances of `Feature`. The simplest is the basic feature or `Price`:

```haskell
type NoParameters = ()

instance Feature Price where
    -- The `Price` feature has no free parameters
    type Parameters Price = NoParameters

    deriveFeature :: Monad m 
                  => NoParameters
                  -> Producer (UTCTime, Price) m ()
                  -> Producer (UTCTime, Price) m ()
    deriveFeatures _ prices = prices
```

This is easy because there are no parameters. What about looking at the price history?

```haskell
newtype PriceHistory 
    = MkPriceHistory (Series UTCTime Price)

newtype NumTicks 
    = MkNumTicks { numTicks :: Int }

instance Feature PriceHistory where
    type Parameters PriceHistory = NumTicks

    deriveFeature :: Monad m 
                  => NumTicks
                  -> Producer (UTCTime, Price) m ()
                  -> Producer (UTCTime, PriceHistory) m ()
    deriveFeature (MkPriceHistoryParameters numTicks) prices
        = prices >-> accumulate numTicks 
                 >-> Pipes.map (\xs -> (maximum $ Series.index xs, MkPriceHistory xs))
        where
            -- out of scope, see end of blog post for link to source
            accumulate :: Functor m 
                       => Int
                       -> Pipe (UTCTime, a) (Series UTCTime a) m () 
            accumulate = (...) 
```

Finally, as an example of the power of this approach, we'll create a strategy which combines two features.

First, we'll extend the `Feature` class to combine two features `a` and `b` into one `(a, b)` feature:

```haskell
instance (Feature a, Feature b) => Feature (a, b) where

    type Parameters (a, b) = (Parameters a, Parameters b)
    
    deriveFeature :: Monad m 
                  => Parameters (a, b)
                  -> Producer (UTCTime, Price)  m ()
                  -> Producer (UTCTime, (a, b)) m ()
    deriveFeature (paramsA, paramsB) prices 
        = Pipes.zipWith (\(k,a) (_, b) -> (k, (a, b))) 
                        (deriveFeature paramsA prices) 
                        (deriveFeature paramsB prices)
```

Second, we'll define a simple strategy that compares the most recent price against the average price of the last 10 ticks:

```haskell
import Data.Series ( fold, mean )

finalStrategy :: Strategy (PriceHistory, Price)
finalStrategy 
    = MkStrategy $ \(MkPriceHistory history, price) 
        -> let avgPrice = fold mean history
            in case price `compare` avgPrice of
                GT -> Sell
                LT -> Buy 10
                EQ -> Hold
```

It is trivial to backtest this strategy like so:

```haskell
backtestFinalStrategy :: Monad m 
                      => Producer (UTCTime, Price) m () 
                      -> m BacktestResults
backtestFinalStrategy = backtestStrategy finalStrategy ( MkNumTicks 10, () ) 
```

and voilà!

## Conclusion

In this post, I have shown you how to define trading strategies with typed feature parametrization, which is a neat use of type families. It takes advantage of the Haskell type system for type safety AND extensibility to easily create new features.

*All code is available in this [Haskell module](/files/trading-strats/TradingStrats.hs).*
