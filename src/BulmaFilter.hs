{-# LANGUAGE OverloadedStrings #-}

-- Pandoc transforms for Bulma's oddities
module BulmaFilter
  ( bulmaTransform,
  )
where

import Data.Text (pack)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (walk)

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

-- Recolors inline code (e.g. variable bames) that has links
-- We have a special case whereby if the code is wrapped in a link,
-- we want the link color to show, rather than dark text color.
--
-- I don't know how to do this with Bulma
inlineLinkedCodeColor :: Inline -> Inline
inlineLinkedCodeColor (Link linkAttrs [Code codeAttrs txt] target) =
  let (identifier, classes, keyvals) = codeAttrs
   in (Link linkAttrs [Code (identifier, classes <> ["has-text-link"], keyvals) txt] target)
inlineLinkedCodeColor x = x

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
bulmaTransform = walk bulmaHeadingTransform . bulmaImagesTransform . walk inlineLinkedCodeColor
