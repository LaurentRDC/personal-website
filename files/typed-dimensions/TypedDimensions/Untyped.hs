--  _______                   _       _ _                          _                 
--  |__   __|                 | |     | (_)                        (_)                
--     | |_   _ _ __   ___  __| |   __| |_ _ __ ___   ___ _ __  ___ _  ___  _ __  ___ 
--     | | | | | '_ \ / _ \/ _` |  / _` | | '_ ` _ \ / _ \ '_ \/ __| |/ _ \| '_ \/ __|
--     | | |_| | |_) |  __/ (_| | | (_| | | | | | | |  __/ | | \__ \ | (_) | | | \__ \
--     |_|\__, | .__/ \___|\__,_|  \__,_|_|_| |_| |_|\___|_| |_|___/_|\___/|_| |_|___/
--         __/ | |                                                                    
--        |___/|_|                                                                                                                       

module TypedDimensions.Untyped ( untypedMaxwellBoltzmannDist ) where


-- Boltzmann constant in Joules/Kelvin
boltzmannConstant_JpK :: Double
boltzmannConstant_JpK = 1.380649e-23


untypedMaxwellBoltzmannDist :: Double -- ^ Particle mass in kilogram
                            -> Double -- ^ Thermodynamic temperature in Kelvin
                            -> Double -- ^ Particle velocity in meters/second
                            -> Double -- ^ Probability density
untypedMaxwellBoltzmannDist mass_kg temp_K velocity_mps 
    = ( mass_kg / (2 * pi * boltzmannConstant_JpK * temp_K) ) ** (3/2) 
    * exp ( - (mass_kg * velocity_mps **2) / (2 * pi * boltzmannConstant_JpK * temp_K) )