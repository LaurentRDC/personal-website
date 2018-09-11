{-# LANGUAGE OverloadedStrings #-}

module BulmaTemplate (mkDefaultTemplate) where

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
    H.meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title "Laurent P. René de Cotret - $title$"
    -- Style sheets
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bulma.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/css/style.css"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/css/syntax.css"
    H.link ! rel "stylesheet" ! type_ "font"     ! href fontUrl
    -- Font awesome icons
    H.link ! rel "stylesheet" ! type_ "text/css" ! href fontAwesomeUrl
    -- Math display
    H.script ! type_ "text/javascript" ! async "" ! src mathjaxUrl $ mempty
    -- Bulma helpers
    H.script ! type_ "text/javascript" ! src "/js/navbar-onclick.js" $ mempty

navigationBar :: H.Html
navigationBar = H.section ! class_ "hero is-primary is-bold" $ do
    --------------------------------------------------------------------------
    H.div ! class_ "hero-head" $
        H.nav ! class_ "navbar is-dark"  ! role "navigation" $ 
            H.div ! class_ "container" $ do
                H.div ! class_ "navbar-brand" $ do
                    H.a ! class_ "navbar-item" ! href "/index.html" $ H.strong $ "Laurent P. René de Cotret"
                    
                    -- toggleBurger function defined in navbar-onclick.js
                    H.span ! class_ "navbar-burger burger" ! A.id "burger" ! A.onclick "toggleBurger()"$ do
                        H.span $ mempty
                        H.span $ mempty
                        H.span $ mempty
                
                H.div ! class_ "navbar-menu" ! A.id "navbarMenu" $ 
                    H.div ! class_ "navbar-end" $
                        forM_ [ ("Home",        "/index.html")
                              , ("Media list",  "/media_list.html")
                              , ("Software",    "/software.html")
                              , ("About",       "/about.html")
                              , ("Archive",     "/archive.html") ] renderLink
    --------------------------------------------------------------------------
    H.div ! class_ "hero-body" $
        H.div ! class_ "container has-text-centered" $
            H.h1 ! class_ "title" $
                "$title$"
    --------------------------------------------------------------------------
    H.div ! class_ "hero-foot" $ 
        H.div ! class_ "container has-text-centered" $
            H.p $ mconcat $ intersperse "   |   " $ links

    where
        renderLink (title, link) = H.a ! class_ "navbar-item" ! href link $ title

        -- Generate an icon + anchor 
        mkSocialLink (icon, link, name) = do
            H.i ! class_ icon $ mempty
            H.a ! href link $ name
        -- Links with icons from font Awesome
        links = fmap mkSocialLink [ ("fas fa-envelope",     "mailto:laurent.decotret@outlook.com",                              " e-mail")
                                  , ("fab fa-github",       "https://github.com/LaurentRDC",                                    " GitHub")
                                  , ("fab fa-linkedin",     "https://www.linkedin.com/in/laurent-p-rené-de-cotret-296b38152/",  " LinkedIn")
                                  , ("fab fa-researchgate", "https://www.researchgate.net/profile/Laurent_Rene_De_Cotret",      " ResearchGate")]

defaultMain :: H.Html
defaultMain = H.main $ do
    H.p $ "$body$"
    -- Add a line break at the end of the body because of templateFooter height
    H.br

-- | Generate a templateFooter markup with a message @s@.
templateFooter :: String -> H.Html
templateFooter s = H.footer ! class_ "footer" $ do
    H.div ! class_ "content has-text-centered" $ do
        H.p $ H.toHtml s

-- | Full default template
-- The templateFooter will be adorned with the message @s@
mkDefaultTemplate :: String -> H.Html
mkDefaultTemplate s = H.docTypeHtml $ do
    defaultHead
    H.body $ do
        navigationBar
        H.div ! class_ "section" $
            H.div ! class_ "container" $
                defaultMain
        
        templateFooter s