--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad                    ((>=>), forM_)
import Data.Maybe                       (fromMaybe)
import Data.Monoid                      ((<>))
import Hakyll
import Hakyll.Images                    ( loadImage
                                        , compressJpgCompiler
                                        , scaleImageCompiler )

-- Hakyll can trip on characters like apostrophes
-- https://github.com/jaspervdj/hakyll/issues/109
import qualified GHC.IO.Encoding                 as E

import           Text.Pandoc.Definition          (Pandoc)
import           Text.Pandoc.Extensions
import           Text.Pandoc.Filter.Pyplot       (plotTransform)
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Walk                (walkM)

import           System.IO

import qualified Data.ByteString                 as B
import qualified Text.Blaze.Html.Renderer.String as St
import           Text.Blaze.Html.Renderer.Utf8   (renderHtmlToByteStringIO)

import           BulmaFilter                     (bulmaTransform)
import           BulmaTemplate                   (mkDefaultTemplate,
                                                  tocTemplate)

import           ReadingTimeFilter               (readingTimeTransform)

import           Data.Time.Calendar              (showGregorian)
import           Data.Time.Clock                 (getCurrentTime, utctDay)

import           Feed                            (feedConfiguration)

-- | syntax highlighting style to use throughout
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = kate

-- We match images down to two levels
-- Images/* and images/*/**
jpgImages = "images/*.jpg" .||. "images/*/**.jpg"
nonJpgImages = (     "images/*/**"
                .||. "images/*"
                ) .&&. complement jpgImages
generatedContent = "generated/**"

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
    let template = mkDefaultTemplate (mconcat ["Page generated on ", today, ". "])
    renderHtmlToByteStringIO (B.writeFile "templates/default.html") template >> putStrLn "  Generated templates\\default.html"

    hakyll $ do
        
        --------------------------------------------------------------------------------
        -- A lot of things can be compied directly
        forM_ ["files/*", "fonts/*", "js/*", nonJpgImages] $ 
            \pattern ->
                match pattern $ do
                    route idRoute
                    compile copyFileCompiler

        -- JPG images are special: they can be compressed
        match jpgImages $ do
            route   idRoute
            compile $ loadImage
                >>= compressJpgCompiler 50
        
        match generatedContent $ do
            route generatedRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler
        
        --------------------------------------------------------------------------------
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
                scientific <- loadAll (fromGlob "projects/scientific/*.md")
                general <- loadAll (fromGlob "projects/*.md")

                let projectsCtx = mconcat [
                          listField "scientific" defaultContext (return scientific)
                        , listField "general" defaultContext (return general)
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
        -- Create RSS feed and Atom feeds
        -- See https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
        forM_ [ ("feed.xml", renderRss)
             , ("atom.xml", renderAtom)
             ] $
            \(name, renderFunc) -> create [name] $ do
                route idRoute
                compile $ do
                    let feedCtx = postCtx <> bodyField "description"
                    posts <- fmap (take 10) . recentFirst =<< 
                        loadAllSnapshots "posts/*" "content"
                    renderFunc feedConfiguration feedCtx posts
        
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
                  , defaultContext
                  ]


-- | Allow math display, code highlighting, table-of-content, and Pandoc filters
-- Note that the Bulma pandoc filter is always applied last
pandocCompiler_ :: Compiler (Item String)
pandocCompiler_ = do
    ident <- getUnderlying
    toc <- getMetadataField ident "withtoc"
    tocDepth <- getMetadataField ident "tocdepth"
    let extensions = defaultPandocExtensions
        writerOptions = case toc of
            Just _ -> defaultHakyllWriterOptions
                { writerExtensions = extensions
                , writerHTMLMathMethod = MathJax ""
                , writerHighlightStyle = Just syntaxHighlightingStyle
                , writerTableOfContents = True
                , writerTOCDepth = read (fromMaybe "3" tocDepth) :: Int
                , writerTemplate = Just $ St.renderHtml tocTemplate
                }
            Nothing -> defaultHakyllWriterOptions
                { writerExtensions = extensions
                , writerHTMLMathMethod = MathJax ""
                , writerHighlightStyle = Just syntaxHighlightingStyle
                }
    -- Pandoc filters are composed in the 'transform' function
    pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        writerOptions
        (unsafeCompiler . transforms)
    
    where
        -- Overall document transform, i.e. the combination
        -- of all Pandoc filters
        transforms doc = bulmaTransform <$> plotTransform doc

-- Pandoc extensions used by the compiler
defaultPandocExtensions :: Extensions
defaultPandocExtensions = 
    let extensions = [ 
            -- Pandoc Extensions: http://pandoc.org/MANUAL.html#extensions
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
        defaultExtensions = writerExtensions defaultHakyllWriterOptions

    in foldr enableExtension defaultExtensions extensions

-- Move content from static/ folder to base folder
staticRoute :: Routes
staticRoute = (gsubRoute "static/" (const ""))

-- Move generated posts from posts/generated to generated/
generatedRoute :: Routes
generatedRoute = gsubRoute "generated/" (const "posts/generated/")
