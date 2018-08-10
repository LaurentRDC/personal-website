---
title: How this website is generated
date: 2018-08-02
updated: 2018-08-05
---

In the hope that this page is useful to someone, I would like to explain how I generate this website.

## Static website generator

I am always looking for an excuse to play with Haskell, and so this website is generated using [Hakyll](https://jaspervdj.be/hakyll/index.html). Hakyll is a static website generator; website configuration is written in Haskell and content is written in Markdown. Hakyll then collects the content, applies templates to it (navigation bar, page footers, etc), and builds a static website in a separate folder. Deploying the website then simply consists in moving the files to the right place.

I use [`stack`](https://docs.haskellstack.org/en/stable/README/) to manage the project. `stack` even has a Hakyll template (`hakyll-template`).

My `site.hs` configuration file is fairly standard, but I want to mention two modifications I have made that might help you as well.

### Math display and syntax highlighting

This idea is borrowed from [JD Reaver](https://github.com/jdreaver/jdreaver.com/)

By default, LaTeX-style math is not rendered properly when posts are consumed by Hakyll. We need to provide `pandoc` - the library that performs the Markdown-to-HTML translation - with a list of extensions to use.

The same problem arises when trying to highlight code. Instead of using the default `pandocCompiler`, I use a slightly modified compiler that passes extra options to `pandoc`.

The type signature of our new compiler `pandocCompiler_` is:

```haskell
pandocCompiler_ :: Compiler (Item String)
```

Exactly the same signature as `pandocCompiler`, what did you expect? The first step is to collect the relevant [Pandoc extensions](http://hackage.haskell.org/package/pandoc-2.2.2.1/docs/Text-Pandoc-Extensions.html), which I have separated for clarity:

```haskell
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
```

The math extensions allows me to write LaTeX code inclusing macros like `\begin{align} ... \end{align}`. The code extensions allow me to easily switch languages for code blocks.

I then collect all the extensions:

```haskell
newExtensions = foldr enableExtension defaultExtensions (mathExtensions <> codeExtensions)
```

Note the use of the `mappend` function (`<>`), which could be replaces with list concatenation `++`. We modify the Pandoc write options to include these extensions:

```haskell
defaultExtensions = writerExtensions defaultHakyllWriterOptions
writerOptions =
    defaultHakyllWriterOptions
    { writerExtensions = newExtensions
    , writerHTMLMathMethod = MathJax ""
    , writerHighlightStyle = Just syntaxHighlightingStyle
    }
```

(Ignore the line `writerHighlightStyle = Just syntaxHighlightingStyle` for now, this is related to syntax highlighting.) Finally, we create a new compiler that takes those write options into account:

```haskell
pandocCompilerWith defaultHakyllReaderOptions writerOptions
```

The full code for this math-and-code compiler is below:

```haskell
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
```

All `pandocCompiler` function calls are replaced with `pandocCompiler_`

### Syntax highlighting style as CSS

Pandoc comes with [eight syntax highlighting styles](http://hackage.haskell.org/package/pandoc-2.2.2.1/docs/Text-Pandoc-Highlighting.html):

* pygments
* espresso
* zenburn
* tango
* kate
* monochrome
* breezeDark
* haddock

For some reason, there was no difference between the different highlighting styles when generating my website. Therefore, I wanted to generate a CSS file for the style I want to use.

First, I pinned the style I want to use:

```haskell
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = haddock
```

You will recognize the use of this in the above `pandocCompiler_`. Next, I want to generate CSS for this style with Pandoc and place it with all other CSS files for this website:

```haskell
generateSyntaxHighlightingCSS :: IO ()
generateSyntaxHighlightingCSS = do
    let css = styleToCss syntaxHighlightingStyle
    writeFile "css/syntax.css" css
    return ()
```

The `syntax.css` file must be included in the HTML template:

```html
<link rel="stylesheet" type="text/css" href="/css/syntax.css" />
```

Finally, before Hakyll does anything, I generate this styling file:

```haskell
main :: IO ()
main = do

    generateSyntaxHighlightingCSS
    
    hakyll $ do ...
```

This way, changing the highlighting style is a simple matter of modifying `syntaxHighlightingStyle`. In the future, I will want to define my own `Style` (monokai).

## Update 2018-08-05 : Generating the HTML templates using Blaze

Hakyll uses HTML templating to enforce consistent styling for all pages of this website. Handwriting HTML is error-prone; therefore, this website uses an HTML template generated using [`blaze-html`](https://hackage.haskell.org/package/blaze-html).

`blaze-html` is a domain-specific language - a library of Haskell functions that mimic HTML. For example, the following `blaze-html` code:

```haskell
import Text.Blaze.Html5 as H

defaultMain :: H.Html
defaultMain = H.main $ do
    H.h1 "$title$"
    "$body$"
```

will render to the following HTML:

```html
<main>
    <h1>$title$</h1>
    $body$
</main>
```

Note that in this case, `$title$` and `$body$` are placeholders that will be replaced with page content, like a blog post title and text. I generate the HTML template before Hakyll goes to work:

```haskell
main :: IO ()
main = do
    -- Generate the CSS required to to syntax highlighting
    let css = styleToCss syntaxHighlightingStyle
    writeFile "css/syntax.css" css

    -- Generate the default HTML template
    let template = renderHtml mkDefaultTemplate
    B.writeFile "templates/default.html" template

    hakyll $ do ...
```