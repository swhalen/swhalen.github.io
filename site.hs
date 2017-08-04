#!/usr/bin/env stack
-- stack --resolver lts-9.0 --install-ghc runghc --package hakyll

{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.List (isSuffixOf)
import qualified Data.Set as S
import Text.Pandoc.Options

import Hakyll (
  Item, Context, Compiler,
  hakyll, match, route, compile, create,
  idRoute, setExtension, complement, (.&&.), (.||.),
  recentFirst, listField, makeItem, fromFilePath,
  biblioCompiler, cslCompiler, copyFileCompiler, templateBodyCompiler,
  load, loadAll, loadAndApplyTemplate, relativizeUrls,
  defaultContext, defaultHakyllReaderOptions, defaultHakyllWriterOptions,
  withUrls, dateField, getResourceString, readPandocBiblio, writePandocWith,
  )


postPattern = "blog/*/*.md" .||. "blog/*/*.tex"


main :: IO ()
main = hakyll $ do
  match "assets/*.bib" $ compile biblioCompiler
  match "assets/*.csl" $ compile cslCompiler

  match "index.html" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile copyFileCompiler

  match postPattern $ do
    route $ setExtension "html"
    compile $ postCompiler
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


postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

-- Compilers

postCompiler :: Compiler (Item String)
postCompiler = do
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


-- Utilities

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)


cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
