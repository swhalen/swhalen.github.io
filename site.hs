{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.List (isSuffixOf)
import qualified Data.Set as S
import Text.Pandoc.Options

import Hakyll


postPattern = "blog/*/*.md" .||. "blog/*/*.tex"


main :: IO ()
main = hakyll $ do
  match "assets/*.bib" $ compile biblioCompiler
  match "assets/*.csl" $ compile cslCompiler

  match "index.html" $ do
    route   idRoute
    compile $ copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match postPattern $ do
    route $ setExtension "html"
    compile $ myPandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= relativizeUrls

  match ("blog/*/*"
         .&&. complement postPattern
         .&&. complement "blog/*/*.metadata") $ do
    route idRoute
    compile copyFileCompiler

  create ["blog/index.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll postPattern
        let blogCtx = listField "posts" postCtx (return posts) `mappend`
                      defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/blog.html" blogCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

  match "templates/*" $ compile templateBodyCompiler


myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
    csl <- load $ fromFilePath "assets/springer-mathphys-brackets.csl"
    bib <- load $ fromFilePath "assets/references.bib"
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_single_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""}
        read = readPandocBiblio defaultHakyllReaderOptions
        write = writePandocWith writerOptions

    fmap write (getResourceString >>= read csl bib)


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
