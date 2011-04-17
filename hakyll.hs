{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Monoid
import Prelude hiding (id)
import System.Locale

import Hakyll

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
            >>> arr (renderDateFieldWith cs "date" "%-d. %B %Y" "Neznámé datum")
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
        route  $ customRoute tagToRoute
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
    newest n = take n . reverse . sortByBaseName

    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = arr sortTags >>> renderTagCloud tagIdentifier 100 200

    sortTags :: Tags String -> Tags String
    sortTags = Tags . sortBy (comparing (map toLower . fst)) . tagsMap

    tagIdentifier :: String -> Identifier
    tagIdentifier = fromCapture "tags/*"

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
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
        >>> applyTemplateCompiler "templates/default.html"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedRoot        = "http://fi.muni.cz/~xsedlar3/"
    , feedTitle       = "~xsedlar3"
    , feedDescription = "home of ~xsedlar3"
    , feedAuthorName  = "Lubomír Sedlář"
    }

tagToRoute :: Identifier -> FilePath
tagToRoute = (++".html") . stripDiacritics . map toLower . toFilePath

stripDiacritics :: String -> String
stripDiacritics str = foldl (\s (f,t) -> replace f t s) str diacritics

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace f r (x:xs)
  | f == x    = r : replace f r xs
  | otherwise = x : replace f r xs

diacritics :: [(Char, Char)]
diacritics = [ ('ě', 'e')
             , ('š', 's')
             , ('č', 'c')
             , ('ř', 'r')
             , ('ž', 'z')
             , ('ý', 'y')
             , ('á', 'a')
             , ('í', 'i')
             , ('é', 'e')
             ]

cs :: TimeLocale
cs = TimeLocale { wDays = [ ("pondělí", "po"), ("úterý", "út"), ("středa", "st"),
                            ("čtvrtek", "čt"), ("pátek", "pá"), ("sobota", "so"),
                            ("neděle", "ne") ]
                , months = [
                    ("leden", "led"), ("únor", "ún"), ("březen", "bře"),
                    ("duben", "dub"), ("květen", "kvě"), ("červen", "čer"),
                    ("červenec", "čec"), ("srpen", "srp"), ("září", "žá"),
                    ("říjen", "ří"), ("listopad", "lis"), ("prosinec", "pro") ]
                , intervals = []
                , amPm = ("am", "pm")
                , dateTimeFmt = ""
                , dateFmt = ""
                , timeFmt = ""
                , time12Fmt = ""
                }
