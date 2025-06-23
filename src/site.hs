{-# LANGUAGE OverloadedStrings #-}

-- Hakyll can trip on characters like apostrophes
-- https://github.com/jaspervdj/hakyll/issues/109

import BulmaFilter (bulmaTransform)
import Control.Monad (forM_)
import Data.ByteString qualified as B
import Data.Char (isSpace)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List qualified as List (intersperse, sort)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Feed (feedConfiguration)
import GHC.IO.Encoding qualified as E
import Hakyll
import Hakyll.Images (
  compressJpgCompiler,
  ensureFitCompiler,
  loadImage,
 )
import ReadingTimeFilter (readingTimeFilter)
import System.FilePath ((</>))
import System.Process.Typed (ExitCode (..), readProcess, shell)
import Template (getAnalyticsTagFromEnv, mkDefaultTemplate, tocTemplate)
import Text.Blaze.Html.Renderer.Pretty qualified as Pretty
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Pandoc.Definition
import Text.Pandoc.Extensions
import Text.Pandoc.Filter.Plot (plotFilter)
import Text.Pandoc.Filter.Plot qualified as P
import Text.Pandoc.Highlighting (Style, kate, styleToCss)
import Text.Pandoc.Options
import Text.Pandoc.Templates qualified as Template

-- | syntax highlighting style to use throughout
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = kate

-- We match images down to two levels
-- Images/* and images/*/**
jpgImages :: Pattern
jpgImages = "images/*.jpg" .||. "images/*/**.jpg"

nonJpgImages :: Pattern
nonJpgImages =
  ( "images/*/**"
      .||. "images/*"
  )
    .&&. complement jpgImages

generatedContent :: Pattern
generatedContent = "generated/**"

postsPattern :: Pattern
postsPattern = ("posts/*" .||. "posts/*/**") .&&. complement draftsPattern

draftsPattern :: Pattern
draftsPattern = "posts/drafts/*" .||. "posts/drafts/*/**"

tagsPattern :: Pattern
tagsPattern = "tags/*.html"

--------------------------------------------------------------------------------

-- | Site configuration
conf :: Configuration
conf =
  defaultConfiguration
    { destinationDirectory = "_rendered"
    , providerDirectory = "."
    }

renderTemplate :: IO B.ByteString
renderTemplate = do
  today <- getCurrentTime <&> (showGregorian . utctDay)
  analyticsTag <- getAnalyticsTagFromEnv
  case analyticsTag of
    Nothing -> putStrLn "  No analytics tag found in environment."
    Just _ -> putStrLn "  Analytics tag found in environment."
  let template = mkDefaultTemplate analyticsTag (mconcat ["Page generated on ", today, ". "])
  return (T.encodeUtf8 . T.pack . Pretty.renderHtml $ template)

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
    forM_ ["files/*", "files/*/**", "fonts/*", "js/*", nonJpgImages] $
      \pattern ->
        match pattern $ do
          route idRoute
          compile copyFileCompiler

    -- JPG images are special: they can be compressed
    match jpgImages $ do
      route idRoute
      compile $
        loadImage
          >>= compressJpgCompiler (50 :: Integer)
          -- Coffee table pictures are pretty large, so
          -- I resize them so they fit in 1920x1080px
          >>= ensureFitCompiler 1920 1080

    match generatedContent $ do
      route generatedRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    --------------------------------------------------------------------------------
    -- These are static pages, like the "about" page
    -- Note that /static/index.html is a special case and is handled below,
    -- just like 404.md
    match ("static/*.md" .&&. complement "static/404.md") $ do
      route $ setExtension "html" `composeRoutes` staticRoute
      compile $
        pandocCompiler_ plotConfig
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    --------------------------------------------------------------------------------
    -- Compile projects page
    -- We need to compile each project individually first
    -- If this is not done, we cannot use the metadata in HTML templates
    match "projects/**.md" $
      compile $
        pandocCompiler_ plotConfig >>= relativizeUrls

    create ["software.html"] $ do
      route idRoute
      compile $ do
        scientific <- loadAll (fromGlob "projects/scientific/*.md")
        general <- loadAll (fromGlob "projects/*.md")

        let projectsCtx =
              mconcat
                [ listField "scientific" defaultContext (return scientific)
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

    tags <-
      -- We are sorting tags because the order determines the presentation
      -- of the tags on the archive page
      sortTagsBy (compare `on` fst)
        <$> buildTags postsPattern (fromCapture tagsPattern)

    match postsPattern $ do
      route $ setExtension "html"
      compile $ do
        -- This weird compilation action is structured so that we can extract the reading time
        -- from the document, and use it in a context
        -- TODO: include Pandoc metainformation to implement reading-time filter
        --       See for example here:
        --            https://github.com/jaspervdj/hakyll/issues/643
        (metaCtx, doc) <- pandocCompilerWithMeta plotConfig
        saveSnapshot "content" doc -- Saved content for RSS feed
          >>= loadAndApplyTemplate "templates/default.html" (postCtx tags <> metaCtx)
          >>= relativizeUrls

    --------------------------------------------------------------------------------
    -- Compile draft blog posts
    --
    -- I want to be able to read them (to see how they are rendered, for example),
    -- but without them being linked anywhere.
    match draftsPattern $ do
      route $ setExtension "html"
      compile $ do
        -- This weird compilation action is structured so that we can extract the reading time
        -- from the document, and use it in a context
        -- TODO: include Pandoc metainformation to implement reading-time filter
        --       See for example here:
        --            https://github.com/jaspervdj/hakyll/issues/643
        (metaCtx, doc) <- pandocCompilerWithMeta plotConfig
        loadAndApplyTemplate "templates/default.html" (postCtx tags <> metaCtx) doc
          >>= relativizeUrls

    --------------------------------------------------------------------------------
    -- Compile tag pages
    tagsRules tags $ \tag pattern -> do
      let title = mconcat ["Posts tagged \"", tag, "\""]
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx =
              mconcat
                [ constField "title" title
                , constField "tag" tag
                , listField "posts" (postCtx tags) (pure posts)
                , defaultContext
                ]

        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    --------------------------------------------------------------------------------
    -- Create RSS feed and Atom feeds
    -- See https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    forM_
      [ ("feed.xml", renderRss)
      , ("atom.xml", renderAtom)
      ]
      $ \(name, renderFunc) -> create [name] $ do
        route idRoute
        compile $ do
          let feedCtx = postCtx tags <> bodyField "description"
          posts <-
            fmap (take 10) . recentFirst
              =<< loadAllSnapshots "posts/*" "content"
          renderFunc feedConfiguration feedCtx posts

    --------------------------------------------------------------------------------
    -- Create a page containing all posts
    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              mconcat
                [ listField "posts" (postCtx tags) (return posts)
                , constField "title" "All blog posts"
                , tagCloudField' "tag-cloud" tags
                , defaultContext
                ]

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    --------------------------------------------------------------------------------
    -- Generate the home page, including recent blog posts
    match "static/index.html" $ do
      route staticRoute
      compile $ do
        posts <- take 10 <$> (recentFirst =<< loadAll "posts/*")
        let indexCtx =
              listField "posts" (postCtx tags) (return posts)
                <> defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    --------------------------------------------------------------------------------
    -- Generate the home page, including recent blog posts
    match "static/404.md" $ do
      route $ setExtension "html" `composeRoutes` staticRoute
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
    -- We don't relativize the URLs for the 404 page such that
    -- links like <root>/wtf/wtf.html link to the right CSS

    --------------------------------------------------------------------------------
    -- Create a sitemap for easier search engine integration
    -- Courtesy of Robert Pearce <https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html>
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        -- Gather all posts
        posts <- recentFirst =<< loadAll "posts/*"
        -- Gather all other pages
        pages <- loadAll (fromGlob "static/**")
        tagPages <- loadAll "tags/*"
        let allPages = pages <> posts <> tagPages
            sitemapCtx =
              constField "root" "https://laurentrdc.xyz/"
                <> listField "pages" (postCtx tags) (return allPages)

        makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    --------------------------------------------------------------------------------
    match "templates/*" $ compile templateCompiler

postCtx :: Tags -> Context String
postCtx tags =
  mconcat
    [ constField "root" "https://laurentrdc.xyz/"
    , dateField "date" "%Y-%m-%d"
    , tagsFieldWith
        (fmap List.sort . getTags) -- sort for presentation
        simpleRenderLink
        (mconcat . List.intersperse ", ")
        "tags"
        tags
    , lastUpdatedField
    , -- Because the template language isn't powerful enough
      -- to compare dates, we need to construct an updated message field
      -- separately.
      updatedMessageField
    , defaultContext
    ]

-- | Check when a file was last updated, based on the git history
lastUpdatedViaGit :: FilePath -> IO (Maybe String)
lastUpdatedViaGit fp = do
  (ec, out, _) <- readProcess (shell $ "git log --follow --date=format:\"%Y-%m-%d\" --format=\"%ad\" -- " <> fp <> " | head -1")
  case ec of
    ExitFailure _ -> return Nothing
    ExitSuccess ->
      return
        . Just
        . TL.unpack
        . TL.strip -- Very important, to remove newlines
        . TL.decodeUtf8
        $ out

{- | Field which provides the "last-updated" variable for items, which
provides the date of the most recent git commit which modifies a file.
Note that this context will be unavailable for generated pages
-}
lastUpdatedField :: Context String
lastUpdatedField = field "updated" $ \(Item ident _) -> unsafeCompiler $ do
  lastUpdated <- lastUpdatedViaGit (toFilePath ident)
  case lastUpdated of
    Nothing -> return "<unknown>"
    Just dt -> return dt

{- | Field which provides the "updatedMessage" variable for items, which
provides the date of the most recent git commit which modifies a file.
Note that this context will be unavailable for generated pages
-}
updatedMessageField :: Context String
updatedMessageField = field "updatedMessage" $ \(Item ident _) -> do
  meta <- getMetadata ident
  unsafeCompiler $ do
    lastUpdated <- lastUpdatedViaGit (toFilePath ident)

    let created = lookupString "date" meta
    if lastUpdated == created
      then pure mempty
      else case lastUpdated of
        Nothing -> pure mempty
        Just dt -> return $ "Last updated on " <> filter (not . isSpace) dt <> "."

-- Pandoc compiler which also provides the Pandoc metadata as template context
-- This is necessary because it is not possible for Hakyll to be
-- aware of Pandoc document metadata at this time.
pandocCompilerWithMeta :: P.Configuration -> Compiler (Context String, Item String)
pandocCompilerWithMeta config = do
  let readerOptions = defaultHakyllReaderOptions

  doc <- traverse (unsafeCompiler . transforms) =<< readPandocWith readerOptions =<< getResourceBody

  let Pandoc meta _ = itemBody doc
      metaCtx = M.foldMapWithKey extractMeta (unMeta meta)

  ident <- getUnderlying
  toc <- getMetadataField ident "withtoc"
  tocDepth <- getMetadataField ident "tocdepth"
  template <-
    unsafeCompiler $
      either error id
        <$> Template.compileTemplate mempty (T.pack . renderHtml $ tocTemplate)

  let extensions = defaultPandocExtensions
      writerOptions = case toc of
        Just _ ->
          defaultHakyllWriterOptions
            { writerExtensions = extensions
            , writerHTMLMathMethod = MathML
            , writerHighlightStyle = Just syntaxHighlightingStyle
            , writerTableOfContents = True
            , writerTOCDepth = read (fromMaybe "3" tocDepth) :: Int
            , writerTemplate = Just template
            }
        Nothing ->
          defaultHakyllWriterOptions
            { writerExtensions = extensions
            , writerHTMLMathMethod = MathML
            , writerHighlightStyle = Just syntaxHighlightingStyle
            }

  return (metaCtx, writePandocWith writerOptions doc)
 where
  transforms doc = bulmaTransform . readingTimeFilter <$> plotFilter config (Just "HTML") doc

  extractMeta :: T.Text -> MetaValue -> Context String
  extractMeta name metavalue =
    case metavalue of
      MetaString txt -> mkField $ T.unpack txt
      _ -> mempty
   where
    mkField = field (T.unpack name) . const . return

-- | Allow math display, code highlighting, table-of-content, and Pandoc filters
pandocCompiler_ ::
  -- | Pandoc-plot configuration
  P.Configuration ->
  Compiler (Item String)
pandocCompiler_ = fmap snd . pandocCompilerWithMeta

-- Pandoc extensions used by the compiler
defaultPandocExtensions :: Extensions
defaultPandocExtensions =
  let extensions =
        [ -- Pandoc Extensions: http://pandoc.org/MANUAL.html#extensions
          -- Math extensions
          Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros
        , -- Code extensions
          Ext_fenced_code_blocks
        , Ext_backtick_code_blocks
        , Ext_fenced_code_attributes
        , Ext_inline_code_attributes -- Inline code attributes (e.g. `<$>`{.haskell})
        -- Markdown extensions
        , Ext_implicit_header_references -- We also allow implicit header references (instead of inserting <a> tags)
        , Ext_definition_lists -- Definition lists based on PHP Markdown
        , Ext_yaml_metadata_block -- Allow metadata to be speficied by YAML syntax
        , Ext_superscript -- Superscripts (2^10^ is 1024)
        , Ext_subscript -- Subscripts (H~2~O is water)
        , Ext_footnotes -- Footnotes ([^1]: Here is a footnote)
        ]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
   in foldr enableExtension defaultExtensions extensions

-- Move content from static/ folder to base folder
staticRoute :: Routes
staticRoute = gsubRoute "static/" (const "")

-- Move generated posts from posts/generated to generated/
generatedRoute :: Routes
generatedRoute = gsubRoute "generated/" (const "posts/generated/")

{- | Modification of `tagCloudField`, to render each link
in a Bulma "column"
-}
tagCloudField' ::
  -- | Destination key
  String ->
  -- | Input tags
  Tags ->
  -- | Context
  Context a
tagCloudField' key =
  tagCloudFieldWith
    key
    (\_ _ tag url _ _ _ -> makeLink tag url)
    unwords
    100
    100
 where
  -- The link creation function below ASSUMES that the
  -- tag is surrounded by a <div class="columns"> ... </div> tag
  -- See here:
  --  https://bulma.io/documentation/columns/responsiveness/
  makeLink :: (String -> String -> String)
  makeLink tag url =
    renderHtml $
      H.a ! A.href (H.toValue url) $
        H.div ! A.class_ "column" $
          H.p ! A.class_ "has-text-centered has-text-weight-bold" $
            mconcat [H.span ! A.class_ "icon has-text-link" $ H.i ! A.class_ "fas fa-tag" $ mempty, " ", H.toHtml tag]
