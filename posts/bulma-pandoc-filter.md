---
title: Example of a Pandoc filter to abstract away CSS framework quirks
date: 2018-09-12
summary: A blog post on using a Pandoc filter to help integrate the Bulma CSS framework into this website. 
tags: meta
---

To make this static website render correctly on both desktop and mobile, I've decided to 'upgrade' my setup to use the [Bulma CSS framework](https://bulma.io). This introduced a problem I did not anticipate.

For example, consider the following "raw" HTML tag to create a level 1 title:

```html
<h1>Title</h1>
```

However, in Bulma, [headings must be of a specific class](https://bulma.io/documentation/elements/title/), like so [^1]:

```html
<!-- Level-1 title -->
<h1 class="title is-1">Title</h1>

<!-- Level-2 title -->
<h2 class="title is-2">Title</h2>

<!-- Level-1 subtitle -->
<h1 class="subtitle is-1">Title</h1>
```

Problem is, a lot of headings included on my website are generated from Markdown to HTML using [Pandoc](http://pandoc.org/). Predictably, Markdown headings like `# Title`{.markdown} are converted to "raw" HTML headings like `<h1>Title</h1>`{.html}, and not the `<h1 class="title is-1">Title</h1>`{.html} that I need to use.

__This is a textbook example of a problem that can be solved with a Pandoc filter.__

During the conversion from Markdown to HTML, Pandoc constructs an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) representing the document. A Pandoc filter is used to include transformations to this abstract syntax tree. This is precisely what we want : we want to transform headings into a slightly different type of headings that will play nicely with Bulma.

There are some examples in the [Pandoc documentation on filters](http://pandoc.org/filters.html), but I would like to document the process I used to create this filter.

We'll be writing the filter in Haskell, because I can then include in directly in the website code generation ([more info here](/posts/making-this-website.html)).

### The Pandoc abstract syntax tree

We need to familiarize ourselves with the Pandoc abstract syntax tree (AST). This is defined in the [`pandoc-types` package](https://hackage.haskell.org/package/pandoc-types), most importantly in the `Text.Pandoc.Definition`{.haskell} module (see [here](https://hackage.haskell.org/package/pandoc-types-1.17.5.1/docs/Text-Pandoc-Definition.html)).

We're using Haskell, so let's look at the data types. A Pandoc document is converted from some source format (in our case, Markdown) to the `Pandoc` type:

```haskell
data Pandoc = Pandoc Meta [Block]
```

Without looking at the details, we can see that a document is a list of blocks as well as some metadata. The `Block`{.haskell} datatype is more interesting:

```haskell
data Block
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Para [Inline]         -- ^ Paragraph
    (...)                   -- (omitted)
    | Header Int Attr [Inline] -- ^ Header - level (integer) and text (inlines)
    (...)                   -- (omitted)
```
(source [here](https://hackage.haskell.org/package/pandoc-types-1.17.5.1/docs/src/Text.Pandoc.Definition.html#Block))

There we go! One of the possible type of blocks is a header. This header has a level (level 1 header is the largest title), some attributes, and `[Inline]`{.haskell} represents the content of the header. We're interested in modifying the header attributes, so let's look at `Attr`{.haskell}:

```haskell
-- | Attributes: identifier, classes, key-value pairs
type Attr = ( String                -- Identifier. Not important
            , [String]              -- ^ class      (e.g. ["a", "b"] -> class="a b" in HTML)
            , [(String, String)])   -- Not important
```



The "classes" part of the attribute is precisely what we'd like to modify. Recall that to get Bulma to work, we want to have headings looking like `<h3 class="title is-3">Title</h3>`{.html}.

### Modifying one AST node

Let's write a function that modifies `Block`{.haskell}s (i.e. one tree node) like we want [^2]:

```haskell
-- This is from the pandoc-types package
import Text.Pandoc.Definition   (Block(..), Attr)

toBulmaHeading :: Block -> Block
-- Pattern matching on the input
-- Any Block that is actually a header should be changed
toBulmaHeading (Header level attrs xs) = Header level newAttrs xs
    where
        (identifier, classes, keyvals) = attrs
        -- We leave identifier and key-value pairs unchanged
        newAttrs = ( identifier
                    -- We extend header classes to have the Bulma classes "title" and "is-*"
                    -- where * is the header level
                   , classes <> ["title", "is-" <> show level]
                   , keyvals)

-- We leave any non-header blocks unchanged
toBulmaHeading x = x
```

### Modifying the entire AST

All we need now is to traverse the entire syntax tree, and modify every block according to the `toBulmaHeading`{.haskell} function. This is trivial using the `Text.Pandoc.Walk.walk`{.haskell} function (also from `pandoc-types`). Thanks to typeclasses, `walk`{.haskell} works on many types, but the one specialization I'm looking for is:

```haskell
walk :: (Block -> Block)    -- ^ A function that modifies the abstract syntax three
     -> Pandoc              -- ^ A syntax tree       
     -> Pandoc              -- ^ Our modified syntax tree
```

Our filter then becomes:

```haskell
-- This is from the pandoc-types package
import Text.Pandoc.Definition   (Pandoc, Block(..), Attr)
import Text.Pandoc.Walk         (walk)

toBulmaHeading :: Block -> Block
toBulmaHeading (Header level attrs xs) = Header level newAttrs xs
    where
        (identifier, classes, keyvals) = attrs
        -- We leave identifier and key-value pairs unchanged
        newAttrs = ( identifier
                    -- We extend header classes to have the Bulma classes "title" and "is-*"
                    -- where * is the header level
                   , classes <> ["title", "is-" <> show level]
                   , keyvals)

-- We leave any non-header blocks unchanged
toBulmaHeading x = x

-- | Pandoc filter that changes headings to play nicely with Bulma
bulmaHeadingTransform :: Pandoc -> Pandoc
bulmaHeadingTransform = walk toBulmaHeading
```

### Hooking into Hakyll

To include this filter in my Hakyll pipeline, I only need to provide this filter to the `pandocCompilerWithTransform`{.haskell} function. Hakyll will then apply the Pandoc filter after the AST has been generated from Markdown, but before HTML rendering happens.

If you want to know how to integrate all of this you can shoot me an e-mail.

## Closing remarks

I hope this example has shown you the process behind writing Pandoc filters. Without modifying the content of my posts, I have been able to integrate Bulma in my static website. 

I could also have done it by replacing Markdown headers with inline HTML. However, this would have been less fun.

You can take a look at the [source code](https://github.com/LaurentRDC/personal-website) used to generate this website.

[^1]: I'm sure there is a way to abstract those details away, but the objective today is to play with Pandoc.

[^2]: I'm using the `mappend`{.haskell} operation `<>`{.haskell} to concatenate lists and strings. I could have used `++`{.haskell}, but `<>`{.haskell} just looks so slick.