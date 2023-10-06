{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((>=>))
import Hakyll
import Text.Pandoc (Block, HTMLMathMethod (MathJax), Pandoc, bottomUpM)
import Text.Pandoc.Diagrams
import Text.Pandoc.Highlighting (Style, styleToCss, tango)
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.SideNote (usingSideNotes)

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
      , writerSectionDivs = True
      }
    (compileDiagrams >=> (return . usingSideNotes))

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

  match ("images/*" .||. "diagrams/*" .||. "et-book/**") $ do
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
    compile
      $ myPandocCompiler
      >>= loadAndApplyTemplate "templates/standalone.html" defaultContext
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
        >>= loadAndApplyTemplate "templates/standalone.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  --------------------------------------------------
  -- Posts
  --------------------------------------------------

  match "posts/*" $ do
    route $ setExtension "html"
    compile
      $ myPandocCompiler
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
        fmap (take 10)
          . recentFirst
          =<< loadAllSnapshots "posts/*" "postContent"
      renderRss feedConfig feedCtx posts

  --------------------------------------------------
  -- Index
  --------------------------------------------------

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/standalone.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

------------------------------------------------------------
-- Old code

-- --------------------------------------------------
-- -- Archive
-- --------------------------------------------------

-- create ["archive.html"] $ do
--   route idRoute
--   compile $ do
--     posts <- recentFirst =<< loadAll "posts/*"
--     let archiveCtx =
--           listField "posts" postCtx (return posts)
--             <> constField "title" "Archives"
--             <> defaultContext

--     makeItem ""
--       >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--       >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--       >>= relativizeUrls

-- --------------------------------------------------
-- -- Pagination
-- --------------------------------------------------

-- -- https://dannysu.com/2015/10/29/hakyll-pagination/
-- -- http://limansky.me/posts/2016-12-28-pagination-with-hakyll.html

-- pag <- buildPaginateWith postsGrouper "posts/*" postsPageId

-- paginateRules pag $ \pageNum pat -> do
--   route idRoute
--   compile $ do
--     posts <- recentFirst =<< loadAllSnapshots pat "postContent"

--     -- do any of the posts use katex?
--     useKatex <-
--       any isJust
--         <$> mapM (\item -> getMetadataField (itemIdentifier item) "katex") posts
--     let paginateCtx = paginateContext pag pageNum
--         indexCtx =
--           constField "title" ("Blog Archive - Page " ++ show pageNum)
--             <> listField "posts" postCtx (return posts)
--             <> paginateCtx
--             <> (if useKatex then constField "katex" "true" else mempty)
--             <> defaultContext
--     makeItem ""
--       >>= loadAndApplyTemplate "templates/page.html" indexCtx
--       >>= loadAndApplyTemplate "templates/default.html" indexCtx
--       >>= relativizeUrls

-- ------------------------------------------------------------
-- -- Pagination
-- ------------------------------------------------------------

-- postsPageId :: PageNumber -> Identifier
-- postsPageId pageNum = fromFilePath $ "page/" ++ show pageNum ++ "/index.html"

-- postsGrouper :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
-- postsGrouper = fmap (paginateEvery 10) . sortRecentFirst
