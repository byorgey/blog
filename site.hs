{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Pandoc (Block, HTMLMathMethod (MathJax), Pandoc, bottomUpM)
import Text.Pandoc.Diagrams
import Text.Pandoc.Highlighting (Style, styleToCss, tango)
import Text.Pandoc.Options (WriterOptions (..))

------------------------------------------------------------
-- Configuration
------------------------------------------------------------

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

feedConfig :: FeedConfiguration
feedConfig =
  FeedConfiguration
    { feedTitle = "blog :: Brent -> [String]"
    , feedDescription = "Brent Yorgey's academic blog"
    , feedAuthorName = "Brent Yorgey"
    , feedAuthorEmail = "byorgey@gmail.com"
    , feedRoot = "http://byorgey.github.io/blog"
    }

-- Learned how to do Haskell syntax highlighting from
-- https://rebeccaskinner.net/posts/2021-01-31-hakyll-syntax-highlighting.html
pandocCodeStyle :: Style
pandocCodeStyle = tango

------------------------------------------------------------
-- Diagrams rendering
------------------------------------------------------------

insertDiagrams' :: Opts -> [Block] -> IO [Block]
insertDiagrams' opts bs = concat <$> mapM (insertDiagrams opts) bs

compileDiagrams :: Pandoc -> Compiler Pandoc
compileDiagrams = unsafeCompiler . bottomUpM (insertDiagrams' diagramOpts)

diagramOpts :: Opts
diagramOpts =
  Opts
    { _backend = SVG
    , _absolutePath = True
    , _expression = "dia"
    , _outDir = "diagrams"
    , _outFormat = ""
    }

------------------------------------------------------------
-- Custom pandoc compiler
------------------------------------------------------------

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      , writerHTMLMathMethod = MathJax ""
      }
    compileDiagrams

------------------------------------------------------------
-- Custom contexts
------------------------------------------------------------

tagCtx :: Tags -> Context String
tagCtx = tagsField "tags"

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

------------------------------------------------------------
-- Rules
------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
  --------------------------------------------------
  -- Templates
  --------------------------------------------------

  match "templates/*" $ compile templateCompiler

  --------------------------------------------------
  -- Static files
  --------------------------------------------------

  match ("images/*" .||. "diagrams/*") $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss pandocCodeStyle

  --------------------------------------------------
  -- Static pages
  --------------------------------------------------

  match (fromList ["about.md", "contact.md"]) $ do
    route $ setExtension "html"
    compile $
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  --------------------------------------------------
  -- Tags
  --------------------------------------------------

  -- https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html

  tags <- buildTags "posts/*" (fromCapture "tag/*.html")

  tagsRules tags $ \tag pat -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx =
            constField "title" title
              <> listField "posts" (tagCtx tags <> postCtx) (return posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  --------------------------------------------------
  -- Posts
  --------------------------------------------------

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" (tagCtx tags <> postCtx)
        >>= saveSnapshot "postContent"
        >>= loadAndApplyTemplate "templates/default.html" (tagCtx tags <> postCtx)
        >>= relativizeUrls

  --------------------------------------------------
  -- RSS feed
  --------------------------------------------------

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots "posts/*" "postContent"
      renderRss feedConfig feedCtx posts

  --------------------------------------------------
  -- Archive
  --------------------------------------------------

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Archives"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  --------------------------------------------------
  -- Index
  --------------------------------------------------

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Home"
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
