{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.List (isSuffixOf)
import Hakyll


main :: IO ()
main = hakyll $ do
  match "index.html" $ do
    route   idRoute
    compile $ copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "blog/*/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= relativizeUrls

  match ("blog/*/*" .&&. complement "blog/*/*.md") $ do
    route idRoute
    compile copyFileCompiler

  create ["blog/index.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "blog/*/*.md"
        let blogCtx = listField "posts" postCtx (return posts) `mappend`
                      defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/blog.html" blogCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

  match "templates/*" $ compile templateBodyCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext


cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)


-- cleanIndexHtmls :: Item String -> Compiler (Item String)
-- cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
--     where
--       pattern = "/index.html"
--       replacement = const "/"


cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
