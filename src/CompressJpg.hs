{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CompressJpg (compressJpgCompiler) where

import Hakyll

-- To compress Jpg files
import Hakyll.Core.Compiler.Internal (compilerProvider, compilerAsk)
import Hakyll.Core.Provider          (resourceFilePath)
import Data.Typeable                 (Typeable)
import Data.Binary                   (Binary(..))

import Codec.Picture                 (readJpeg, saveJpgImage)

-- Compressing Jpg Files
newtype JpgFile = JpgFile FilePath
    deriving (Binary)

type JpgQuality = Int

instance Writable JpgFile where
    write dst (Item _ (JpgFile src)) = compressJpg src dst 25

-- | Copy a JPG, but compress to a certain quality setting
-- The quality should be between 0 and 100
compressJpg :: FilePath -> FilePath -> JpgQuality -> IO ()
compressJpg src dst quality = do
    im <- readJpeg src
    case im of
        Right im -> saveJpgImage quality dst im
        Left _ -> error $
            "Loading the image " <> show src <> " failed."

compressJpgCompiler :: Compiler (Item JpgFile)
compressJpgCompiler = do
    identifier <- getUnderlying
    provider   <- compilerProvider <$> compilerAsk
    makeItem $ JpgFile $ resourceFilePath provider identifier