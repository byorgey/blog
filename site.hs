{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Pandoc.Highlighting (Style, styleToCss, tango)
import Text.Pandoc.Options (WriterOptions (..))

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

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      }

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss pandocCodeStyle

  match (fromList ["about.md", "contact.md"]) $ do
    route $ setExtension "html"
    compile $
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= saveSnapshot "postContent"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

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

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots "posts/*" "postContent"
      renderRss feedConfig feedCtx posts

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext
