{-# LANGUAGE OverloadedStrings #-}
module ReadingTimeFilter (
      readingTimeFilter
) where

import           Data.Map                 (insert)
import           Data.Monoid              (Sum(..))
import qualified Data.Text                as T
import           Text.Pandoc.Definition   (Pandoc(..), Block(..), Caption(..), Inline(..), Meta(..), MetaValue(..))
import           Text.Pandoc.Walk         (query)

import           Text.Printf              (printf)

-- | Page reading time in minutes
type ReadingTime = Double

-- | Number of words in a document
type WordCount = Sum Int 

wordCount :: Pandoc -> WordCount
wordCount = query wordCountBlock
    where
        wordCountBlock :: Block -> WordCount
        wordCountBlock (Plain xs)          = mconcat $ wordCountInline <$> xs
        wordCountBlock (Para xs)           = mconcat $ wordCountInline <$> xs
        wordCountBlock (LineBlock xs)      = mconcat $ mconcat $ fmap wordCountInline <$> xs
        wordCountBlock (CodeBlock _ s)     = Sum $ 3 * length (T.words s)
        wordCountBlock (RawBlock _ s)      = Sum $ length (T.words s)
        wordCountBlock (BlockQuote xs)     = mconcat $ wordCountBlock <$> xs
        wordCountBlock (OrderedList _ xs)  = mconcat $ mconcat $ fmap wordCountBlock <$> xs
        wordCountBlock (BulletList xs)     = mconcat $ mconcat $ fmap wordCountBlock <$> xs
        wordCountBlock (DefinitionList xs) = mconcat [ mconcat (wordCountInline <$> ils) <> mconcat (mconcat $ fmap wordCountBlock <$> bls) | (ils, bls) <- xs ]
        wordCountBlock (Header _ _ xs)     = mconcat $ wordCountInline <$> xs
        wordCountBlock HorizontalRule      = mempty
        wordCountBlock (Table {})          = Sum 200 -- Assuming roughly 1 min of reading time
        wordCountBlock (Figure _ (Caption _ xs) ys)  
                                           = mconcat (wordCountBlock <$> xs) <> mconcat (wordCountBlock <$> ys)
        wordCountBlock (Div _ xs)          = mconcat $ wordCountBlock <$> xs


        wordCountInline :: Inline -> WordCount
        wordCountInline (Str s)          = Sum $ length $ T.words s
        wordCountInline (Emph xs)        = mconcat $ wordCountInline <$> xs
        wordCountInline (Underline xs)   = mconcat $ wordCountInline <$> xs
        wordCountInline (Strong xs)      = mconcat $ wordCountInline <$> xs
        wordCountInline (Strikeout xs)   = mconcat $ wordCountInline <$> xs
        wordCountInline (Superscript xs) = mconcat $ wordCountInline <$> xs
        wordCountInline (Subscript xs)   = mconcat $ wordCountInline <$> xs
        wordCountInline (SmallCaps xs)   = mconcat $ wordCountInline <$> xs
        wordCountInline (Quoted _ xs)    = mconcat $ wordCountInline <$> xs
        wordCountInline (Cite _ xs)      = mconcat $ wordCountInline <$> xs
        wordCountInline (Code _ s)       = Sum $ 3 * length (T.words s)
        wordCountInline Space            = 0 
        wordCountInline SoftBreak        = 0 
        wordCountInline LineBreak        = 0 
        wordCountInline (Math _ s)       = Sum $ length $ T.words s
        wordCountInline (RawInline _ s)  = Sum $ length $ T.words s
        wordCountInline (Link _ xs _)    = mconcat $ wordCountInline <$> xs
        wordCountInline (Image _ s _)    = Sum 200 <> mconcat (wordCountInline <$> s)
        wordCountInline (Note _)         = 0
        wordCountInline (Span _ xs)      = mconcat $ wordCountInline <$> xs


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