--   _______                   _       _ _                          _                 
--  |__   __|                 | |     | (_)                        (_)                
--     | |_   _ _ __   ___  __| |   __| |_ _ __ ___   ___ _ __  ___ _  ___  _ __  ___ 
--     | | | | | '_ \ / _ \/ _` |  / _` | | '_ ` _ \ / _ \ '_ \/ __| |/ _ \| '_ \/ __|
--     | | |_| | |_) |  __/ (_| | | (_| | | | | | | |  __/ | | \__ \ | (_) | | | \__ \
--     |_|\__, | .__/ \___|\__,_|  \__,_|_|_| |_| |_|\___|_| |_|___/_|\___/|_| |_|___/
--         __/ | |                                                                    
--        |___/|_|                                                                    
                               
{-# LANGUAGE NoImplicitPrelude #-}
module TypedDimensions.Typed ( 
    maxwellBoltzmannDist, 
    forDiatomicNitrogen,
    forDiatomicNitrogenFunnyUnits,
 ) where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI ( nauticalMile, degreeRankine, knot, unifiedAtomicMassUnit )
import Numeric.NumType.DK.Integers (TypeInt(..))


boltzmannConstant :: Quantity DHeatCapacity Double
boltzmannConstant = 1.380649e-23 *~ (joule / kelvin)


type DVelocityCube = DVelocity ^ Pos3
type DVelocityDensity = Recip DVelocityCube

type VelocityDensity = Quantity DVelocityDensity


raiseToThreeHalfsPower :: Floating a 
                       => Quantity d a 
                       -> Quantity (NRoot d Pos2 ^ Pos3) a
raiseToThreeHalfsPower x = (sqrt x) ^ pos3


maxwellBoltzmannDist :: Mass Double
                     -> ThermodynamicTemperature Double
                     -> Velocity Double
                     -> VelocityDensity Double
maxwellBoltzmannDist mass temp velocity 
    = raiseToThreeHalfsPower ( mass / (_2 * pi * boltzmannConstant * temp) )
    * exp ( negate (mass * velocity ^ pos2) / (_2 * pi * boltzmannConstant * temp) )


forDiatomicNitrogen :: VelocityDensity Double
forDiatomicNitrogen 
    = let n2_mass = 2 *~ unifiedAtomicMassUnit
          room_temperature = 300.0 *~ kelvin
          velocity = 400 *~ (meter / second)
       in (maxwellBoltzmannDist n2_mass room_temperature velocity)


forDiatomicNitrogenFunnyUnits :: Double
forDiatomicNitrogenFunnyUnits
    = let n2_mass = 2.6605E-27 *~ kilo gram
          room_temperature = 491.0 *~ degreeRankine
          velocity = 777 *~ knot
       in (maxwellBoltzmannDist n2_mass room_temperature velocity) /~ ((hour / nauticalMile) ^ pos3)