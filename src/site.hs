--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad                    (forM_)
import Data.Maybe                       (fromMaybe)
import Hakyll
import Hakyll.Images                    ( loadImage
                                        , compressJpgCompiler
                                        )

-- Hakyll can trip on characters like apostrophes
-- https://github.com/jaspervdj/hakyll/issues/109
import qualified GHC.IO.Encoding                 as E

import qualified Text.DocTemplates               as D
import           Text.Pandoc.Extensions
import           Text.Pandoc.Filter.Plot         (plotTransform)
import qualified Text.Pandoc.Filter.Plot         as P
import           Text.Pandoc.Highlighting        (Style, styleToCss, kate)
import           Text.Pandoc.Options
import qualified Text.Pandoc.Templates           as Template

import           Text.Printf                     (printf)

import qualified Data.ByteString                 as B
import qualified Data.Map.Strict                 as M

import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T

import           System.FilePath                 ((</>))

import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty

import           BulmaFilter                     (bulmaTransform)
import           Template                        (mkDefaultTemplate,
                                                  tocTemplate)

import           Data.Time.Calendar              (showGregorian)
import           Data.Time.Clock                 (getCurrentTime, utctDay)

import           Feed                            (feedConfiguration)
import           ReadingTimeFilter               (readingTime)

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
-- | Site configuration
conf :: Configuration
conf = defaultConfiguration
        { destinationDirectory = "_rendered"
        , providerDirectory = "."
        }

renderTemplate :: IO B.ByteString
renderTemplate = do
    today <- getCurrentTime >>= return . showGregorian . utctDay
    let template = mkDefaultTemplate (mconcat ["Page generated on ", today, ". "])
    return template
        -- We need to go through Text because of utf8-encoding
        >>= return . T.encodeUtf8 . T.pack . Pretty.renderHtml
        
--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Hakyll can trip on characters like apostrophes
    -- https://github.com/jaspervdj/hakyll/issues/109
    E.setLocaleEncoding E.utf8

    plotConfig <- P.configuration ".pandoc-plot.yml"

    -- generate the CSS required to to syntax highlighting
    let css = styleToCss syntaxHighlightingStyle
    writeFile ("css" </> "syntax.css") css >> putStrLn "  Generated css/syntax.css"

    -- We generate the default template
    -- The template has a marking showing on what date was the page generated
    renderTemplate 
        >>= B.writeFile ("templates" </> "default.html") 
        >> putStrLn "  Generated templates/default.html"

    hakyllWith conf $ do
            
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
            compile $ pandocCompiler_ plotConfig
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        --------------------------------------------------------------------------------
        -- Compile projects page
        -- We need to compile each project individually first
        -- If this is not done, we cannot use the metadata in HTML templates
        match ("projects/**.md") $ compile $ pandocCompiler_ plotConfig >>= relativizeUrls

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
            compile $ do
                -- This weird compilation action is structured so that we can extract the reading time
                -- from the document, and use it in a context
                -- TODO: include Pandoc metainformation to implement reading-time filter
                --       See for example here:
                --            https://github.com/jaspervdj/hakyll/issues/643
                (rt, a) <- pandocCompilerWithRT plotConfig
                let postCtxWithRT = postCtx <> constField "reading-time" rt
                return a
                    >>= saveSnapshot "content"  -- Saved content for RSS feed
                    >>= loadAndApplyTemplate "templates/default.html" postCtxWithRT
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
                        constField "root" "https://laurentrdc.xyz/" <>
                        listField "pages" postCtx (return allPages)

                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

        --------------------------------------------------------------------------------
        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat [ defaultContext
                  , constField "root" "https://laurentrdc.xyz/"
                  , dateField "date" "%Y-%m-%d"
                  ] 

-- Pandoc compiler which also bundles reading time
-- This is necessary because it is not possible for Hakyll to be
-- aware of Pandoc document metadata at this time.
pandocCompilerWithRT :: P.Configuration -> Compiler (String, Item String)
pandocCompilerWithRT config = do
    let readerOptions = defaultHakyllReaderOptions

    doc <- traverse (unsafeCompiler . transforms) =<< readPandocWith readerOptions =<< getResourceBody 
    let rt = readingTime $ itemBody doc

    ident <- getUnderlying
    toc <- getMetadataField ident "withtoc"
    tocDepth <- getMetadataField ident "tocdepth"
    template <- unsafeCompiler $ (either error id) <$> 
                    Template.compileTemplate mempty (T.pack . renderHtml $ tocTemplate)

    let extensions = defaultPandocExtensions
        writerOptions = case toc of
            Just _ -> defaultHakyllWriterOptions
                { writerExtensions = extensions
                , writerHTMLMathMethod = MathJax ""
                , writerHighlightStyle = Just syntaxHighlightingStyle
                , writerTableOfContents = True
                , writerTOCDepth = read (fromMaybe "3" tocDepth) :: Int
                , writerTemplate = Just template
                }
            Nothing -> defaultHakyllWriterOptions
                { writerExtensions = extensions
                , writerHTMLMathMethod = MathJax ""
                , writerHighlightStyle = Just syntaxHighlightingStyle
                }

    return (printf "%.0f" rt, writePandocWith writerOptions doc)
    where
        transforms doc = bulmaTransform <$> plotTransform config doc

-- | Allow math display, code highlighting, table-of-content, and Pandoc filters
pandocCompiler_ :: P.Configuration        -- ^ Pandoc-plot configuration
                -> Compiler (Item String)
pandocCompiler_ conf = do
    (_, a) <- pandocCompilerWithRT conf
    return a

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
