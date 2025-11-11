{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad ((<=<), (>=>))
import Data.List (findIndex, intersperse)
import Hakyll
import Text.Pandoc (Block, HTMLMathMethod (MathJax), Pandoc, bottomUpM)
import Text.Pandoc.Diagrams
import Text.Pandoc.Highlighting (Style, tango)
import Text.Pandoc.Options (CiteMethod (..), ReaderOptions, WriterOptions (..))
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
-- Bibliography
------------------------------------------------------------

-- https://tony-zorman.com/posts/hakyll-and-bibtex.html

processBib :: Item Pandoc -> Compiler (Item Pandoc)
processBib pandoc = do
  csl <- load "bib/chicago.csl"
  bib <- load "bib/references.bib"
  processPandocBiblio csl bib pandoc

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
  myPandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      , writerHTMLMathMethod = MathJax ""
      , writerSectionDivs = True
      , writerCiteMethod = Citeproc
      }
    (processBib >=> traverse compileDiagrams >=> traverse (pure . usingSideNotes))

-- https://tony-zorman.com/posts/hakyll-and-bibtex.html

myRenderPandocWithTransformM ::
  ReaderOptions ->
  WriterOptions ->
  (Item Pandoc -> Compiler (Item Pandoc)) -> -- this changed!
  Item String ->
  Compiler (Item String)
myRenderPandocWithTransformM ropt wopt f i =
  writePandocWith wopt <$> (f =<< readPandocWith ropt i)

myPandocCompilerWithTransformM ::
  ReaderOptions ->
  WriterOptions ->
  (Item Pandoc -> Compiler (Item Pandoc)) -> -- this changed!
  Compiler (Item String)
myPandocCompilerWithTransformM ropt wopt f =
  getResourceBody >>= myRenderPandocWithTransformM ropt wopt f

------------------------------------------------------------
-- Custom contexts
------------------------------------------------------------

tagCtx :: Tags -> Context String
tagCtx = tagsFieldWith getTagsAndCategories simpleRenderLink (mconcat . intersperse ", ") "tags"

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

------------------------------------------------------------
-- Prev & next posts
------------------------------------------------------------

-- Adapted from https://hrothen.github.io/posts/switching-from-jekyll-bootstrap-to-hakyll.html

lookupWithOffset :: Int -> [a] -> (a -> Bool) -> Maybe a
lookupWithOffset off as p = findIndex p as >>= (as !?) . (+ off)

-- This exists in base-4.19 Data.List
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n

------------------------------------------------------------
-- Tags + categories
------------------------------------------------------------

getTagsAndCategories :: MonadMetadata m => Identifier -> m [String]
getTagsAndCategories ident =
  liftA2 (++) (getTagsByField "tags" ident) (getTagsByField "categories" ident)

------------------------------------------------------------
-- Rules
------------------------------------------------------------

postPattern :: Pattern
postPattern = "posts/**.markdown" .||. "posts/**.md" .||. "posts/**.lhs"

main :: IO ()
main = hakyllWith config $ do
  --------------------------------------------------
  -- Templates
  --------------------------------------------------

  match "templates/*" $ compile templateCompiler

  --------------------------------------------------
  -- Static files
  --------------------------------------------------

  match ("images/*" .||. "diagrams/*" .||. "et-book/**" .||. "posts/**/images/*") $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- Used to create syntax.css like this, but now hard-coded the file
  -- in order to add a dark mode.

  -- create ["css/syntax.css"] $ do
  --   route idRoute
  --   compile $ makeItem $ styleToCss pandocCodeStyle

  match "bib/*.csl" $ compile cslCompiler
  match "bib/*.bib" $ compile biblioCompiler
  --------------------------------------------------
  -- Static pages
  --------------------------------------------------

  match (fromList ["about.md", "contact.md"]) $ do
    route $ setExtension "html"
    compile $
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/standalone.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  --------------------------------------------------
  -- Tags
  --------------------------------------------------

  -- https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html

  tags <- buildTagsWith getTagsAndCategories postPattern (fromCapture "tag/*.html")

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

  postList <- sortRecentFirst =<< getMatches postPattern

  match postPattern $ do
    route $ setExtension "html"
    compile $ do
      let prevPostID = lookupWithOffset 1 postList . (==) . itemIdentifier
          nextPostID = lookupWithOffset (-1) postList . (==) . itemIdentifier
          idToURL :: Identifier -> Compiler String
          idToURL = fmap toUrl . maybeNoResult <=< getRoute
          idToTitle :: Identifier -> Compiler String
          idToTitle i = getMetadataField i "title" >>= maybeNoResult
          maybeNoResult :: Maybe a -> Compiler a
          maybeNoResult = maybe (noResult "no such post") pure
          postLocationContext =
            mconcat
              [ field "prevurl" (idToURL <=< (maybeNoResult . prevPostID))
              , field "nexturl" (idToURL <=< (maybeNoResult . nextPostID))
              , field "prevtitle" (idToTitle <=< (maybeNoResult . prevPostID))
              , field "nexttitle" (idToTitle <=< (maybeNoResult . nextPostID))
              ]
          finalPostCtx = postLocationContext <> tagCtx tags <> postCtx

      myPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" finalPostCtx
        >>= saveSnapshot "postContent"
        >>= loadAndApplyTemplate "templates/nextprev.html" finalPostCtx
        >>= loadAndApplyTemplate "templates/default.html" finalPostCtx
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
          =<< loadAllSnapshots postPattern "postContent"
      renderRss feedConfig feedCtx posts

  --------------------------------------------------
  -- Index
  --------------------------------------------------

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postPattern
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
--     posts <- recentFirst =<< loadAll postPattern
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

-- pag <- buildPaginateWith postsGrouper postPattern postsPageId

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
