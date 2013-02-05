{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.Monoid
import Prelude hiding (id)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Renderer.String (renderHtml)


import Hakyll
import Text.Pandoc
import Czech

main :: IO ()
main = hakyll $ do

    match "css/*" $ do
        route   $ setExtension "css"
        compile $ getResourceString
            >>> unixFilter "sass" ["-s", "-C", "-t", "compressed", "--scss"]

    match "static/*" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/static.html"
            >>> defaultCompiler

    forM_ ["favicon.ico", "data/*", "images/*"] $ \p ->
        match p $ do
            route   idRoute
            compile copyFileCompiler

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pageCompilerWith defaultHakyllParserState myWriterOptions
            >>> arr (renderCzechDate "date")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> defaultCompiler

    match "posts.html" $ do
        route  idRoute
        create "posts.html" $
            constA mempty
                >>> arr (setField "title" "Všechny texty")
                >>> requireAllA "posts/*" addPostList
                >>> applyTemplateCompiler "templates/posts.html"
                >>> defaultCompiler

    match "index.html" $ do
        route  idRoute
        create "index.html" $
            constA mempty
                >>> arr (setField "title" "Index of ~xsedlar3")
                >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
                >>> requireAllA "posts/*" (second (arr $ newest 5) >>> addPostList)
                >>> applyTemplateCompiler "templates/index.html"
                >>> defaultCompiler

    forM_ ["403.html", "404.html"] $ \p ->
        match p $ do
            route   idRoute
            compile $ readPageCompiler
                >>> arr (setField "title" "Chyba na ~xsedlar3")
                >>> defaultCompiler

    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    match "tags/*" $ do
        route  $ customRoute tagToRoute `composeRoutes` setExtension "html"
        metaCompile $ require_ "tags"
            >>> arr tagsMap
            >>> arr (map $ \(t,p) -> (tagIdentifier t, makeTagList t p))

    match "templates/*" $ compile templateCompiler

    match "rss.xml" $ do
        route  idRoute
        create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

    return ()

  where
    newest :: Int -> [Page a] -> [Page a]
    newest n = take n . reverse . chronological

    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = sortTagsBy caseInsensitiveTags
                    >>> renderTagCloud tagIdentifier 100 200

    tagIdentifier :: String -> Identifier a
    tagIdentifier = fromCapture "tags/*"

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions
    { writerHtml5 = True
    }

defaultCompiler :: Compiler (Page String) (Page String)
defaultCompiler = applyTemplateCompiler "templates/default.html"
              >>> relativizeUrlsCompiler

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Texty označené jako " ++ tag))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> defaultCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedRoot        = "http://fi.muni.cz/~xsedlar3/"
    , feedTitle       = "~xsedlar3"
    , feedDescription = "home of ~xsedlar3"
    , feedAuthorName  = "Lubomír Sedlář"
    }

tagToRoute :: Identifier a -> FilePath
tagToRoute = stripDiacritics . map toLower . toFilePath
