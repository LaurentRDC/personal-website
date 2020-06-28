{-# LANGUAGE OverloadedStrings #-}
-- Pandoc transforms for Bulma's oddities
module BulmaFilter (
    bulmaTransform
) where

import           Data.Text              (pack)
import           Text.Pandoc.Definition (Attr, Block (..), Inline (..), Pandoc)
import           Text.Pandoc.Walk       (walk)


-- | Transform (or filter) to format heading to Bulma's heading classes.
-- Markdown: ## Title
-- HTML    : <h2 class="title is-2">Title</h2>
toBulmaHeading :: Block -> Block
toBulmaHeading (Header level attrs xs) = Header level newAttrs xs
    where
        (identifier, classes, keyvals) = attrs
        newAttrs = (identifier, classes <> ["title", "is-" <> (pack . show $ level)], keyvals)
toBulmaHeading x = x


-- | Take images and add the bulma "image" class to it
-- Markdown : ![](images/wtv.jpg)
-- Html     : <img src="images/wtv.jpg" class = "image"/>
toBulmaImage :: Inline -> Inline
toBulmaImage (Image attrs xs target) = Image newAttrs xs target
    where
        (identifier, classes, keyvals) = attrs
        newAttrs = (identifier, classes <> ["image"], keyvals)
toBulmaImage x = x


-- ! Transform (or filter) to format heading to Bulma's heading classes.
-- Markdown: ## Title
-- HTML    : <h2 class="title is-2">Title</h2>
bulmaHeadingTransform :: Pandoc -> Pandoc
bulmaHeadingTransform = walk toBulmaHeading

-- Take images and add the bulma "image" class to it
-- Markdown : ![](images/wtv.jpg)
-- Html     : <img src="images/wtv.jpg" class = "image"/>
bulmaImagesTransform :: Pandoc -> Pandoc
bulmaImagesTransform = walk toBulmaImage

-- Combination of all transforms
bulmaTransform :: Pandoc -> Pandoc
bulmaTransform = bulmaHeadingTransform . bulmaImagesTransform
