{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Monoid
import Prelude hiding (id)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Renderer.String (renderHtml)


import Hakyll
import Czech

main :: IO ()
main = hakyll $ do

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    forM_ ["favicon.ico", "data/*", "images/*"] $ \p ->
        match p $ do
            route   idRoute
            compile copyFileCompiler

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> arr (renderCzechDate "date")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "posts.html" $ do
        route  idRoute
        create "posts.html" $
            constA mempty
                >>> arr (setField "title" "Všechny texty")
                >>> requireAllA "posts/*" addPostList
                >>> applyTemplateCompiler "templates/posts.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    match "tasks/*" $ compile pageCompiler

    match "tasks.html" $ do
        route  idRoute
        create "tasks.html" $
            constA mempty
                >>> arr (setField "title" "Programovací úlohy")
                >>> requireAllA "tasks/*" addTaskList
                >>> applyTemplateCompiler "templates/tasks.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    match "index.html" $ do
        route  idRoute
        create "index.html" $
            constA mempty
                >>> arr (setField "title" "Index of ~xsedlar3")
                >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
                >>> requireAllA "posts/*" (second (arr $ newest 5) >>> addPostList)
                >>> applyTemplateCompiler "templates/index.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    forM_ ["403.html", "404.html"] $ \p ->
        match p $ do
            route   idRoute
            compile $ readPageCompiler
                >>> arr (setField "title" "Chyba na ~xsedlar3")
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    match "tags/*" $ do
        route  $ customRoute tagToRoute `composeRoutes` setExtension "html"
        metaCompile $ require_ "tags"
            >>> arr tagsMap
            >>> arr (map (\(t,p) -> (tagIdentifier t, makeTagList t p >>> relativizeUrlsCompiler)))

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

    sortTagsBy :: ((String, [Page a]) -> (String, [Page a]) -> Ordering)
               -> Compiler (Tags a) (Tags a)
    sortTagsBy f = arr $ Tags . sortBy f . tagsMap

    caseInsensitiveTags :: (String, [Page a]) -> (String, [Page a]) -> Ordering
    caseInsensitiveTags = comparing $ map toLower . fst

    tagIdentifier :: String -> Identifier a
    tagIdentifier = fromCapture "tags/*"

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

sections :: Page String -> Page String
sections page = foldl doSec page (zip keys parts)
  where
    doSec pg (key, text) = setField key (renderHidingBox key text) pg
    parts = splitAll "<!-- SECTION -->" (pageBody page) ++ repeat ""
    keys = ["task", "hint", "solution"]

renderHidingBox :: String -> String -> String
renderHidingBox "task" t = t
renderHidingBox _ ""     = ""
renderHidingBox h t = renderHtml $
    H.div H.! H.class_ cls $ do
        H.b   $ H.toHtml header
        H.div $ H.preEscapedString t
  where
    cls = H.toValue $ "rounded hiding-box " ++ map toLower h
    header :: String
    header = case h of
                "hint"     -> "Nápověda"
                "solution" -> "Řešení"
                _          -> ""

addTaskList :: Compiler (Page String, [Page String]) (Page String)
addTaskList = setFieldA "tasks" $
    arr chronological
        >>> arr (map sections)
        >>> require "templates/task.html" (\p t -> map (applyTemplate t) p)
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
        >>> applyTemplateCompiler "templates/default.html"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedRoot        = "http://fi.muni.cz/~xsedlar3/"
    , feedTitle       = "~xsedlar3"
    , feedDescription = "home of ~xsedlar3"
    , feedAuthorName  = "Lubomír Sedlář"
    }

tagToRoute :: Identifier a -> FilePath
tagToRoute = stripDiacritics . map toLower . toFilePath
