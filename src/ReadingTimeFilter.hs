module ReadingTimeFilter (
      readingTime
) where

import Data.Monoid              (Sum(..))
import qualified Data.Text      as T
import Text.Pandoc
import Text.Pandoc.Definition   (Pandoc, Inline(..))
import Text.Pandoc.Walk         (query)

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
        _wordCount Image {}         = Sum 100 -- Assuming it takes ~40sec to look at an image
        _wordCount _                = 0

readingTime :: Pandoc -> ReadingTime
readingTime = (/ wordsPerMinute) . realToFrac . getSum . wordCount
    where
        wordsPerMinute = 150
