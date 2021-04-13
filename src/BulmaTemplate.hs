{-# LANGUAGE OverloadedStrings #-}

module BulmaTemplate ( mkDefaultTemplate
                     , tocTemplate
                     ) where

import           Control.Monad               (forM_)

import           Data.List                   (intersperse)

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

fontAwesomeURL = "https://use.fontawesome.com/releases/v5.15.1/css/all.css"
academiconsURL = "https://cdn.rawgit.com/jpswalsh/academicons/master/css/academicons.min.css"
mathjaxURL     = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"
fontURL        = "https://fonts.googleapis.com/css?family=Titillium+Web"

type Icon = String
type URL = String

type SocialLink = (Icon, URL, String)
data NavigationLink = Link URL String

type Schema = [NavigationLink]


schema :: Schema
schema = [
      Link "/index.html"        "Home"
    , Link "/software.html"     "Software"
    , Link "/publications.html" "Publications"
    , Link "/about.html"        "About me"
    , Link "/archive.html"      "Blog posts"
    ]


socialLinks :: [SocialLink]
socialLinks = [
      ("fas fa-envelope",      "mailto:laurent.decotret@outlook.com",                              "e-mail")
    , ("fab fa-github",        "https://github.com/LaurentRDC",                                    "GitHub")
    , ("fab fa-linkedin",      "https://www.linkedin.com/in/laurent-p-rené-de-cotret-296b38152/",  "LinkedIn")
    , ("ai ai-researchgate",   "https://www.researchgate.net/profile/Laurent_Rene_De_Cotret",      "ResearchGate")
    , ("ai ai-orcid",          "https://orcid.org/0000-0002-1464-2739",                            "OrcID")
    , ("ai ai-google-scholar", "https://scholar.google.ca/citations?user=pXFhwioAAAAJ&hl=en",      "Google Scholar")
    ]

feedLinks :: [SocialLink]
feedLinks = [
      ("fas fa-rss",  "/feed.xml", "RSS feed")
    , ("fas fa-atom", "/atom.xml", "Atom feed")
    ]


styleSheets :: [AttributeValue]
styleSheets =
    [ "/css/syntax.css"
    , "/css/style.css"
    , fontAwesomeURL
    , academiconsURL
    ]

defaultHead :: H.Html
defaultHead = H.head $ do
    H.meta ! charset "utf-8"
    H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title "$title$ - Laurent P. René de Cotret"
    -- Tab icon
    -- Note : won't show up for Edge while on localhost
    H.link ! rel "icon" ! type_ "image/x-icon" ! href "/images/atom-solid.png"
    -- Style sheets
    forM_ styleSheets (\link -> H.link ! rel "stylesheet" ! type_ "text/css" ! href link)
    -- Font
    H.link ! rel "stylesheet" ! type_ "font" ! href fontURL
    -- Math display
    H.script ! type_ "text/javascript" ! async "" ! src mathjaxURL $ mempty
    -- Bulma helpers
    H.script ! type_ "text/javascript" ! src "/js/navbar-onclick.js" $ mempty

-- Note that the top-level class is extended via sass to have
-- a background image.
navigationBar :: H.Html
navigationBar = H.section ! class_ ("hero-with-background is-dark") $ do
    --------------------------------------------------------------------------
    H.div ! class_ "hero-head" $
        H.nav ! class_ "navbar is-transparent" $
            H.div ! class_ "container" $ do
                H.div ! class_ "navbar-brand" $ do
                    H.a ! class_ "navbar-item has-text-light" ! href "/index.html" $ H.strong $ "Laurent P. René de Cotret"

                    -- toggleBurger function defined in navbar-onclick.js
                    H.span ! class_ "navbar-burger burger has-text-light" ! A.id "burger" ! A.onclick "toggleBurger()"$ do
                        H.span $ mempty
                        H.span $ mempty
                        H.span $ mempty

                H.div ! class_ "navbar-menu" ! A.id "navbarMenu" $
                    H.div ! class_ "navbar-end" $
                        forM_ schema renderLink
    --------------------------------------------------------------------------
    H.div ! class_ "hero-body" $
        H.div ! class_ "container has-text-centered" $ do
            H.h1 ! class_ "title has-text-light" $
                "$title$"
            H.h3 $ "$if(date)$Posted on $date$$endif$$if(updated)$ | Last updated on $updated$$endif$"
            H.h3 $ "$if(reading-time)$Estimated reading time of $reading-time$ min.$endif$"
    --------------------------------------------------------------------------
    H.div ! class_ "hero-foot" $
        H.div ! class_ "navbar is-transparent" $
            H.div ! class_ "container" $
                H.div ! class_ "navbar-menu" $
                    H.div ! class_ "navbar-start" $
                        forM_ socialLinks mkSocialLink

    where
        renderLink :: NavigationLink -> Html
        renderLink (Link url title) = H.a ! class_ "navbar-item" ! href (toValue url) $ toMarkup title

        -- Generate an icon + anchor
        mkSocialLink (icon, link, name) = H.a ! class_ "navbar-item has-text-light" ! target "_blank" ! href (toValue link) $ do
            H.span ! class_ "icon is-medium" $ H.i ! class_ (toValue icon) $ mempty
            toMarkup $ name

defaultFooter :: String -> H.Html
defaultFooter s = H.footer ! class_ "footer" $
    H.div ! class_ "content has-text-centered" $ do
        -- List all socials links
        -- This is important because on mobile, the hero-foot disappears
        H.p $ (mconcat . intersperse " | ") $ renderLink <$> socialLinks

        H.p $ (mconcat . intersperse " | ") $ renderLink <$> feedLinks

        -- Message and disclaimer
        H.p ! class_ "is-small" $ mconcat
                    [ H.toHtml s
                    , "This website was created using free and open-source technologies. "
                    , "You can learn more about how this website is generated "
                    , (H.a ! href "/about-site.html" $ "here")
                    , "."
                    ]
        H.a ! rel "license" ! href "http://creativecommons.org/licenses/by-sa/4.0/" $ 
            H.img ! alt "Creative Commons License" 
                  ! A.style "border-width:0" 
                  ! src "/images/cc-by-sa.svg"
    where
        renderLink (icon, link, name) = do
            H.span ! class_ "icon" $ H.i ! class_ (toValue icon) $ mempty
            H.a ! target "_blank" ! href (toValue link) $ toMarkup name

-- | Full default template
-- The templateFooter will be adorned with the message @s@
mkDefaultTemplate :: String -> H.Html
mkDefaultTemplate s = H.docTypeHtml $ do
    defaultHead
    H.body $ do
        navigationBar
        H.div ! class_ "section" $
            H.div ! class_ "container" $
            -- Note : the "content" class handles all barebones HTML tags
            -- See here:
            --      https://bulma.io/documentation/elements/content/
                H.div ! class_ "content" $
                    "$body$"

        defaultFooter s

-- Wrap the content of a page with a table of content
tocTemplate :: H.Html
tocTemplate = do
    H.div ! class_ "message is-link" $
        H.div ! class_ "message-body" $ do
            H.h2 $ "On this page"
            H.p $ "$toc$"

    "$body$"
