--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Hakyll
import Hakyll.Images                    (compressJpgCompiler)

-- Hakyll can trip on characters like apostrophes
-- https://github.com/jaspervdj/hakyll/issues/109
import qualified GHC.IO.Encoding as E

import Text.Pandoc.Options
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting 
import Text.Pandoc.Filter.Pyplot        (makePlot)
import Text.Pandoc.Walk                 (walkM)
import Text.Pandoc.Definition           (Pandoc)

import System.IO

import qualified Data.ByteString.Lazy   as B -- Must use lazy bytestrings because of renderHTML
import Text.Blaze.Html.Renderer.Utf8    (renderHtml)

import BulmaTemplate                    (mkDefaultTemplate)
import BulmaFilter                      (bulmaTransform)

import ReadingTimeFilter                (readingTimeTransform)

import Data.Time.Clock                  (getCurrentTime, utctDay)
import Data.Time.Calendar               (showGregorian)

import Feed                             (feedConfiguration)

-- | syntax highlighting style to use throughout
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = kate

-- We match images down to two levels
-- Images/* and images/*/**
jpgImages = "images/*.jpg" .||. "images/*/**.jpg" 
nonJpgImages = (     "images/*/**" 
                .||. "images/*"
                ) .&&. complement jpgImages
generatedContent = "generated/*"

--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Hakyll can trip on characters like apostrophes
    -- https://github.com/jaspervdj/hakyll/issues/109
    E.setLocaleEncoding E.utf8
    

    putStrLn "File generation"

    -- generate the CSS required to to syntax highlighting
    let css = styleToCss syntaxHighlightingStyle
    writeFile "css/syntax.css" css >> putStrLn "  Generated css\\syntax.css"

    -- We generate the default template
    -- The template has a marking showing on what date was the page generated
    today <- getCurrentTime >>= return . showGregorian . utctDay
    let template = renderHtml $ mkDefaultTemplate (mconcat ["Page generated on ", today, ". "])
    B.writeFile "templates/default.html" template >> putStrLn "  Generated templates\\default.html"

    hakyll $ do
        
        -- These are general files like theses, preprints
        match "files/*" $ do
            route   idRoute
            compile copyFileCompiler
        
        -- JPG images are special: they can be compressed
        match jpgImages $ do
            route   idRoute
            compile (compressJpgCompiler 50)

        -- All other images are copied directly
        match nonJpgImages $ do
            route   idRoute
            compile copyFileCompiler
        
        match generatedContent $ do
            route   generatedRoute
            compile copyFileCompiler
        
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler
        
        match "js/*" $ do
            route   idRoute
            compile copyFileCompiler
        
        -- The fonts/ folder is required by academicons
        -- see academicons.css
        match "fonts/*" $ do
            route   idRoute
            compile copyFileCompiler
        
        -- These are static pages, like the "about" page
        -- Note that /static/index.html is a special case and is handled below
        match "static/*.md" $ do
            route $ (setExtension "html") `composeRoutes` staticRoute
            -- No Pandoc filters beyond the built-in bulmaTransform
            -- hence the use of 'id'
            compile $ pandocCompiler_
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
        
        --------------------------------------------------------------------------------
        -- Compile projects page
        -- We need to compile each project individuallycfirst
        -- If this is not done, we cannot use the metadata in HTML templates
        match ("projects/**") $ compile $ pandocCompiler_ >>= relativizeUrls

        create ["software.html"] $ do
            route idRoute
            compile $ do
                projects <- loadAll (fromGlob "projects/*.md")

                let projectsCtx = mconcat [
                          listField "projects" defaultContext (return projects)
                        , constField "title" "Software projects"
                        , defaultContext
                        ]
                
                makeItem "" 
                    >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
                    >>= loadAndApplyTemplate "templates/default.html" projectsCtx
                    >>= relativizeUrls

        
        --------------------------------------------------------------------------------
        -- Compile blog posts
        -- Explicitly do not match the drafts
        match ("posts/*" .&&. complement "posts/drafts/*") $ do
            route $ setExtension "html"
            compile $ pandocCompiler_
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= saveSnapshot "content"  -- Saved content for RSS feed
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls
        
        --------------------------------------------------------------------------------
        -- Create RSS feed
        -- See https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
        create ["feed.xml"] $ do
            route idRoute
            compile $ do
                let feedCtx = postCtx <> bodyField "description"
                posts <- fmap (take 10) . recentFirst =<< 
                    loadAllSnapshots "posts/*" "content"
                renderRss feedConfiguration feedCtx posts

        --------------------------------------------------------------------------------
        -- Create Atom feed
        -- See https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
        create ["atom.xml"] $ do
            route idRoute
            compile $ do
                let feedCtx = postCtx <> bodyField "description"
                posts <- fmap (take 10) . recentFirst =<< 
                    loadAllSnapshots "posts/*" "content"
                renderAtom feedConfiguration feedCtx posts
        
        --------------------------------------------------------------------------------
        -- Create a page containing all posts
        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) <>
                        constField "title" "Blog posts"          <>
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

        --------------------------------------------------------------------------------
        -- Generate the home page, including recent blog posts
        match "static/index.html" $ do
            route staticRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) <>
                        constField "title" "Welcome to my homepage" <>
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        --------------------------------------------------------------------------------
        -- Create a sitemap for easier search engine integration
        -- Courtesy of Robert Pearce <https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html>
        create ["sitemap.xml"] $ do
            route   idRoute
            compile $ do
                -- Gather all announcements
                posts <- recentFirst =<< loadAll "posts/*"
                -- Gather all other pages
                pages <- loadAll (fromGlob "static/**")
                let allPages = pages <> posts
                    sitemapCtx = 
                        constField "root" "http://www.physics.mcgill.ca/~decotret" <>
                        listField "pages" postCtx (return allPages)

                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
        
        --------------------------------------------------------------------------------
        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat [ constField "root" "http://www.physics.mcgill.ca/~decotret/"
                  , dateField "date" "%Y-%m-%d"
                  , defaultContext ]

plotTransform :: Pandoc -> IO Pandoc
plotTransform = walkM makePlot

-- Overall document transform, i.e. the combination
-- of all Pandoc filters
transforms :: Pandoc -> IO Pandoc
transforms doc = bulmaTransform <$> plotTransform doc

-- | Allow math display, code highlighting, and Pandoc filters
-- Note that the Bulma pandoc filter is always applied last
pandocCompiler_ :: Compiler (Item String)
pandocCompiler_ =
    let 
    -- Pandoc Extensions: http://pandoc.org/MANUAL.html#extensions
    extensions = [ 
        -- Math extensions
          Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros
            -- Code extensions
        , Ext_fenced_code_blocks
        , Ext_backtick_code_blocks
        , Ext_fenced_code_attributes        
        , Ext_inline_code_attributes        -- Inline code attributes (e.g. `<$>`{.haskell})
            -- Markdown extensions
        , Ext_implicit_header_references    -- We also allow implicit header references (instead of inserting <a> tags)
        , Ext_definition_lists              -- Definition lists based on PHP Markdown
        , Ext_yaml_metadata_block           -- Allow metadata to be speficied by YAML syntax
        , Ext_superscript                   -- Superscripts (2^10^ is 1024) 
        , Ext_subscript                     -- Subscripts (H~2~O is water)
        , Ext_footnotes                     -- Footnotes ([^1]: Here is a footnote)
        ]
    newExtensions = foldr enableExtension defaultExtensions extensions
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    writerOptions = defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathJax ""
        , writerHighlightStyle = Just syntaxHighlightingStyle
        }
    -- Pandoc filters are composed in the 'transform' function
    in pandocCompilerWithTransformM 
        defaultHakyllReaderOptions 
        writerOptions 
        (unsafeCompiler . transforms)

-- Move content from static/ folder to base folder
staticRoute :: Routes
staticRoute = (gsubRoute "static/" (const ""))

-- Move generated posts from posts/generated to generated/
generatedRoute :: Routes
generatedRoute = gsubRoute "generated/" (const "posts/generated/")