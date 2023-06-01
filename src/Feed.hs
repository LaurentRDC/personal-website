-- RSS Feed configuration
module Feed (
    feedConfiguration
) where

import           Hakyll (FeedConfiguration (..))

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Laurent's personal blog"
    , feedDescription = "My personal blog on science, math, programming, and other interests."
    , feedAuthorName  = "Laurent P. René de Cotret"
    , feedAuthorEmail = mempty
    , feedRoot        = "https://laurentrdc.xyz"
    }
