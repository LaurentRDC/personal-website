--               _  _    _                               _        
--  _ __   _  _ | || |_ (_)__ __ ___  _ _  ___ ___      | |_   ___
-- | '  \ | || || ||  _|| |\ V // -_)| '_|(_-// -_)  _  |   \ (_-/
-- |_|_|_| \_._||_| \__||_| \_/ \___||_|  /__/\___| (_) |_||_|/__/
--
--
-- Tested with GHC 9.0.1
import           Data.Set ( Set, (\\) ) -- from containers
import qualified Data.Set as Set        -- from containers


data Possibilities a 
    = Possibilities [a]
    deriving Show


possibly :: [a] -> Possibilities a
possibly = Possibilities


certainly :: a -> Possibilities a
certainly x = Possibilities [x]


-- A foldable instance for list conversion and `length`
instance Foldable Possibilities where
    foldMap m (Possibilities xs) = foldMap m xs


instance Functor Possibilities where
    fmap f (Possibilities as) = Possibilities (fmap f as)


instance Applicative Possibilities where
    pure = certainly

    -- This is basically the Applicative instance for lists already
    (Possibilities fs) <*> (Possibilities ps) = Possibilities $ [f p | f <- fs, p <- ps]


instance Monad Possibilities where
    
    Possibilities ps >>= f = Possibilities $ concat [toList (f p) | p <- ps]
        where toList (Possibilities xs) = xs
 

data Person = Driver1    | Driver2    | Driver3
            | Passenger1 | Passenger2 | Passenger3
            | Passenger4 | Passenger5
    deriving (Bounded, Ord, Eq, Enum, Show)


assignDriver :: Set Person -> Possibilities (Person, Set Person)
assignDriver people = possibly [ (driver, Set.delete driver people) 
                               | driver <- Set.toList $ people `Set.intersection` possibleDrivers
                               ]
    where possibleDrivers = Set.fromList [Driver1, Driver2, Driver3]


assign3Passengers :: Set Person -> Possibilities (Set Person, Set Person)
assign3Passengers people = possibly [ (passengers, people \\ passengers) 
                                   | passengers <- Set.toList setsOf3
                                   ]
    where setsOf3 = Set.filter (\s -> length s == 3) $ Set.powerSet people


data CarAssignment 
    = CarAssignment { driver1        :: Person
                    , driver2        :: Person
                    , car1Passengers :: Set Person
                    , car2Passengers :: Set Person
                    }
    deriving Show


assignPeopleToCars :: Possibilities CarAssignment
assignPeopleToCars = do
    let everyone = Set.fromList $ enumFromTo minBound maxBound -- [Driver1, Driver2, ..., Passenger6]

    (driver1, rest) <- assignDriver everyone
    (driver2, rest) <- assignDriver rest

    (car1Passengers, rest) <- assign3Passengers rest
    (car2Passengers, _)    <- assign3Passengers rest

    return $ CarAssignment driver1 driver2 car1Passengers car2Passengers


main :: IO ()
main = do

    let carAssignments = assignPeopleToCars
    putStrLn $ "Size of the multiverse: " <> show (length carAssignments)
