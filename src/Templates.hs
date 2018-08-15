{-# LANGUAGE OverloadedStrings #-}

module Templates (mkDefaultTemplate) where

import Data.List (intersperse)
import Control.Monad                (forM_)
import Text.Blaze.Html5             as H
import Text.Blaze.Html5.Attributes  as A

fontAwesomeUrl = "https://use.fontawesome.com/releases/v5.2.0/css/all.css" 
mathjaxUrl = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=default"
fontUrl = "https://fonts.googleapis.com/css?family=Titillium+Web"

defaultHead :: H.Html
defaultHead = H.head $ do
    H.meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
    H.title "Laurent P. René de Cotret - $title$"
    -- Style sheets
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/css/style.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/css/syntax.css"
    H.link ! rel "stylesheet" ! type_ "font"     ! href fontUrl
    -- Font awesome icons
    H.link ! rel "stylesheet" ! type_ "text/css" ! href fontAwesomeUrl
    -- Math display
    H.script ! type_ "text/javascript" ! async "" ! src mathjaxUrl $ mempty

defaultHeader :: H.Html
defaultHeader = H.header $ do
    -- Logo
    H.div ! class_ "logo" $
        H.a ! href "/index.html" $ "Laurent P. René de Cotret"
    
    -- Navigation bar
    H.nav $ forM_ [ ("Home",        "/index.html")
                  , ("Media list",  "/media_list.html")
                  , ("Software",    "/software.html")
                  , ("About",       "/about.html")
                  , ("Archive",     "/archive.html") ] renderLink
    where
        renderLink (title, link) = H.a ! href link $ title

defaultMain :: H.Html
defaultMain = H.main $ do
    H.h1 "$title$"
    "$body$"
    -- Add a line break at the end of the body because of footer height
    H.br

-- | Generate a footer markup with a message @s@.
defaultFooter :: String -> H.Html
defaultFooter s = H.footer $ do
    -- Lists of icons with links
    H.p $ mconcat $ intersperse " | " $ links <> [orcid]
    H.p $ H.toHtml s
    where
        -- Generate an icon + anchor 
        mkLink (icon, link, name) = do
            H.i ! class_ icon $ mempty
            H.a ! href link $ name
        -- Links with icons from font Awesome
        links = fmap mkLink [ ("fas fa-envelope",     "mailto:laurent.decotret@outlook.com",                              " e-mail")
                            , ("fab fa-github",       "https://github.com/LaurentRDC",                                    " GitHub")
                            , ("fab fa-linkedin",     "https://www.linkedin.com/in/laurent-p-rené-de-cotret-296b38152/",  " LinkedIn")
                            , ("fab fa-researchgate", "https://www.researchgate.net/profile/Laurent_Rene_De_Cotret",      " ResearchGate")]
        -- Note that ORCiD doesn't have a FontAwesome icon, so we use a separate function
        orcid = do
            H.a ! href "https://orcid.org/0000-0002-1464-2739" ! target "orcid.widget" ! A.style "vertical-align:top;" $ do
                H.img ! src "https://orcid.org/sites/default/files/images/orcid_16x16.png" ! A.style "width:1em;margin-right:.5em;" <> "ORCiD"

-- | Full default template
-- The footer will be adorned with the message @s@
mkDefaultTemplate :: String -> H.Html
mkDefaultTemplate s = H.docTypeHtml $ do
    defaultHead
    H.body $ do
        defaultHeader
        defaultMain
        defaultFooter s