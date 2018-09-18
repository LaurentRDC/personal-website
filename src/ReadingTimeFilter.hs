module ReadingTimeFilter (
      readingTimeTransform
    , readingTimeTransformMeta
) where

import Text.Pandoc
import Text.Pandoc.Definition   (Pandoc(..), Inline(..), Block(..), Meta(..), MetaValue(..))
import Text.Pandoc.Walk         (query)

import Data.Monoid              (Sum(..))
import Data.Map                 (insert)

-- | Page reading time in minutes
type ReadingTime = Int

-- | Number of words in a document
type WordCount = Sum Int

_wordCount :: Inline -> WordCount
_wordCount (Str s)              = Sum $ length $ words s
_wordCount (Emph xs)            = mconcat $ _wordCount <$> xs
_wordCount (Strong xs)          = mconcat $ _wordCount <$> xs
_wordCount (Strikeout xs)       = mconcat $ _wordCount <$> xs
_wordCount (Superscript xs)     = mconcat $ _wordCount <$> xs
_wordCount (Subscript xs)       = mconcat $ _wordCount <$> xs
_wordCount (SmallCaps xs)       = mconcat $ _wordCount <$> xs
_wordCount (Quoted qtype xs)    = mconcat $ _wordCount <$> xs
_wordCount (Cite citations xs)  = mconcat $ _wordCount <$> xs
_wordCount (Span attrs xs)      = mconcat $ _wordCount <$> xs
_wordCount (Code attrs s)       = Sum $ length $ words s
_wordCount (RawInline f s)      = Sum $ length $ words s
_wordCount _                    = 0

wordCount :: Pandoc -> WordCount
wordCount = query _wordCount

-- | Insert a paragraph at the beginning of the document
-- estimating the reading time in minutes.
readingTimeTransform :: Pandoc -> Pandoc
readingTimeTransform (Pandoc meta blocks) = Pandoc meta ([newBlock] <> blocks)
    where
        nwords = getSum $ wordCount (Pandoc meta blocks)
        readingTime = show $ nwords `div` 100
        newBlock = Para [
            Emph [
                Str $ mconcat [ "This posts contains "
                              , show nwords 
                              , " words. Estimated reading time of "
                              , readingTime
                              , " minutes."]
                ]
            ]

-- | Insert the 'reading-time' metadata key
readingTimeTransformMeta :: Pandoc -> Pandoc
readingTimeTransformMeta (Pandoc meta blocks) = Pandoc newMeta blocks
    where
        nwords = getSum $ wordCount (Pandoc meta blocks)
        readingTime = show $ nwords `div` 100
        newMeta = Meta $ insert "reading-time" (MetaInlines [Str readingTime]) (unMeta meta)