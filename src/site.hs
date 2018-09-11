--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings          #-}

import Data.Monoid ((<>))
import Hakyll

-- Hakyll can trip on characters like apostrophes
-- https://github.com/jaspervdj/hakyll/issues/109
import qualified GHC.IO.Encoding as E

import Text.Pandoc.Options
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting 

import System.IO

import Monokai   (monokai)

import qualified Data.ByteString.Lazy as B -- Must use lazy bytestrings because of renderHTML
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import BulmaTemplate (mkDefaultTemplate)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian, showGregorian)

import CompressJpg (compressJpgCompiler)

-- TODO: RSS feed
-- TODO: generating CSS with Cassius?

-- | syntax highlighting style to use throughout
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = kate

-- We match images down to two levels
-- Images/* and images/*/**
jpgImages = "images/*.jpg" .||. "images/*/**.jpg"
nonJpgImages = ("images/*/**" .||. "images/*") .&&. complement jpgImages

--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Hakyll can trip on characters like apostrophes
    -- https://github.com/jaspervdj/hakyll/issues/109
    E.setLocaleEncoding E.utf8
    
    -- First step is to generate the CSS required to to syntax highlighting
    -- Next, we generate the default template
    -- The template has a marking showing on what date was the page generated
    putStrLn "File generation"

    let css = styleToCss syntaxHighlightingStyle
    writeFile "css/syntax.css" css >> putStrLn "  Generated css\\syntax.css"

    today <- getCurrentTime >>= return . showGregorian . utctDay
    let template = renderHtml $ mkDefaultTemplate ("Page generated on " <> today)
    B.writeFile "templates/default.html" template >> putStrLn "  Generated templates\\default.html"

    hakyll $ do

        match "files/*" $ do
            route   idRoute
            compile copyFileCompiler
        
        -- JPG images are special: they can be compressed
        match jpgImages $ do
            route   idRoute
            compile (compressJpgCompiler 25)

        -- All other images are copied directly
        match nonJpgImages $ do
            route   idRoute
            compile copyFileCompiler
        
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler
        
        match "js/*" $ do
            route   idRoute
            compile copyFileCompiler
            
        match "static/*.md" $ do
            route $ (setExtension "html") `composeRoutes` staticRoute
            compile $ pandocCompiler_
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ pandocCompiler_
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) <>
                        constField "title" "Archives"            <>
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "static/index.html" $ do
            route staticRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) <>
                        constField "title" "Home"                <>
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat [ dateField "date" "%B %e, %Y"
                  , defaultContext ]

-- Allow math display and code highlighting
-- Pandoc Extensions: http://pandoc.org/MANUAL.html#extensions
pandocCompiler_ :: Compiler (Item String)
pandocCompiler_ =
    let
    mathExtensions =
        [ Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros
        ]
    codeExtensions = 
        [ Ext_fenced_code_blocks
        , Ext_backtick_code_blocks
        , Ext_fenced_code_attributes
        , Ext_inline_code_attributes        -- Inline code attributes (e.g. `<$>`{.haskell})
        ]
    markdownExtensions = 
        [ Ext_implicit_header_references    -- We also allow implicit header references (instead of inserting <a> tags)
        , Ext_definition_lists              -- Definition lists based on PHP Markdown
        , Ext_yaml_metadata_block           -- Allow metadata to be speficied by YAML syntax
        , Ext_superscript                   -- Superscripts (2^10^ is 1024) 
        , Ext_subscript                     -- Subscripts (H~2~O is water)
        , Ext_footnotes                     -- Footnotes ([^1]: Here is a footnote)
        ]
    newExtensions = foldr enableExtension defaultExtensions (mconcat [mathExtensions, codeExtensions, markdownExtensions])
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    writerOptions =
        defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathJax ""
        , writerHighlightStyle = Just syntaxHighlightingStyle
        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- Move content from static/ folder to base folder
staticRoute :: Routes
staticRoute = (gsubRoute "static/" (const ""))