{-# LANGUAGE OverloadedStrings #-}
module ReadingTimeFilter (
      readingTimeFilter
) where

import           Data.Map                 (insert)
import           Data.Monoid              (Sum(..))
import qualified Data.Text                as T
import           Text.Pandoc
import           Text.Pandoc.Definition   (Pandoc, Inline(..), Meta(..))
import           Text.Pandoc.Walk         (query)

import           Text.Printf              (printf)

-- | Page reading time in minutes
type ReadingTime = Double

-- | Number of words in a document
type WordCount = Sum Int 

wordCount :: Pandoc -> WordCount
wordCount = query _wordCount
    where
        _wordCount :: Inline -> WordCount
        _wordCount (Str s)          = Sum $ length $ T.words s
        _wordCount (Emph xs)        = mconcat $ _wordCount <$> xs
        _wordCount (Strong xs)      = mconcat $ _wordCount <$> xs
        _wordCount (Strikeout xs)   = mconcat $ _wordCount <$> xs
        _wordCount (Superscript xs) = mconcat $ _wordCount <$> xs
        _wordCount (Subscript xs)   = mconcat $ _wordCount <$> xs
        _wordCount (SmallCaps xs)   = mconcat $ _wordCount <$> xs
        _wordCount (Quoted _ xs)    = mconcat $ _wordCount <$> xs
        _wordCount (Cite _ xs)      = mconcat $ _wordCount <$> xs
        _wordCount (Span _ xs)      = mconcat $ _wordCount <$> xs
        _wordCount (Code _ s)       = Sum $ length $ T.words s
        _wordCount (RawInline _ s)  = Sum $ length $ T.words s
        _wordCount (Math _ s)       = Sum $ length $ T.words s
        -- Word equivalent of the image itself included
        _wordCount (Image _ s _)    = Sum 100 <> mconcat (_wordCount <$> s)
        _wordCount _                = 0

readingTime :: Pandoc -> ReadingTime
readingTime = (/ wordsPerMinute) . realToFrac . getSum . wordCount
    where
        -- M. Brysbaert, How many words do we read per minute? A review and meta-analysis of reading rate,
        -- Journal of Memory and Language (2009) vol 109. DOI: 10.1016/j.jml.2019.104047
        wordsPerMinute = 238

-- | Reads a document and inserts the estimated reading time in minutes
-- in the metadata, under the key "reading-time"
readingTimeFilter :: Pandoc -> Pandoc
readingTimeFilter doc@(Pandoc meta blocks) = Pandoc newMeta blocks
    where
        rt = T.pack $ printf "%.0f" $ readingTime doc
        newMeta = Meta $ insert "reading-time" (MetaString rt) (unMeta meta)