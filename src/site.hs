--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll

import Text.Pandoc.Options
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting 

import System.IO

import Monokai   (monokai)

import qualified Data.ByteString.Lazy as B -- Must use lazy bytestrings because of renderHTML
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Templates (mkDefaultTemplate)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian, showGregorian)

-- TODO: RSS feed
-- TODO: generating CSS with Cassius?

-- | syntax highlighting style to use throughout
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = tango

--------------------------------------------------------------------------------
main :: IO ()
main = do
    
    -- First step is to generate the CSS required to to syntax highlighting
    let css = styleToCss syntaxHighlightingStyle
    writeFile "css/syntax.css" css

    -- Next, we generate the default template
    -- The template has a marking showing on what date was the page generated
    today <- getCurrentTime >>= return . showGregorian . utctDay
    let template = renderHtml $ mkDefaultTemplate ("Page generated on " <> today)
    B.writeFile "templates/default.html" template

    hakyll $ do

        match ("images/*" .||. "files/*") $ do
            route   idRoute
            compile copyFileCompiler
        
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler
        
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
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

-- Allow math display and code highlighting
-- https://github.com/jdreaver/jdreaver.com/blob/master/hakyll.hs
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
        ]
    newExtensions = foldr enableExtension defaultExtensions (mathExtensions <> codeExtensions)
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