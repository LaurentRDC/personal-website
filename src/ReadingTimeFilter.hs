module ReadingTimeFilter (
      readingTimeTransformMeta
) where

import           Text.Pandoc
import           Text.Pandoc.Definition (Block (..), Inline (..), Meta (..),
                                         MetaValue (..), Pandoc (..))
import           Text.Pandoc.Walk       (query)

import           Data.Map               (insert)
import           Data.Monoid            (Sum (..))

-- | Page reading time in minutes
type ReadingTime = Int

-- | Number of words in a document
type WordCount = Sum Int

_wordCount :: Inline -> WordCount
_wordCount (Str s)             = Sum $ length $ words s
_wordCount (Emph xs)           = mconcat $ _wordCount <$> xs
_wordCount (Strong xs)         = mconcat $ _wordCount <$> xs
_wordCount (Strikeout xs)      = mconcat $ _wordCount <$> xs
_wordCount (Superscript xs)    = mconcat $ _wordCount <$> xs
_wordCount (Subscript xs)      = mconcat $ _wordCount <$> xs
_wordCount (SmallCaps xs)      = mconcat $ _wordCount <$> xs
_wordCount (Quoted qtype xs)   = mconcat $ _wordCount <$> xs
_wordCount (Cite citations xs) = mconcat $ _wordCount <$> xs
_wordCount (Span attrs xs)     = mconcat $ _wordCount <$> xs
_wordCount (Code attrs s)      = Sum $ length $ words s
_wordCount (RawInline f s)     = Sum $ length $ words s
_wordCount _                   = 0

wordCount :: Pandoc -> WordCount
wordCount = query _wordCount

-- | Insert the 'reading-time' metadata key
readingTimeTransformMeta :: Pandoc -> Pandoc
readingTimeTransformMeta (Pandoc meta blocks) = Pandoc newMeta blocks
    where
        nwords = getSum $ wordCount (Pandoc meta blocks)
        readingTime = show $ nwords `div` 100
        newMeta = Meta $ insert "reading-time" (MetaString readTimeText) (unMeta meta)
        readTimeText = mconcat [ 
            "Estimated reading time of ", readingTime, " minutes (", show nwords," words)."
            ]
