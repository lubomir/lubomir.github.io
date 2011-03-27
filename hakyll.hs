{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Category (id)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid
import Prelude hiding (id)
import System.Locale

import Hakyll

main :: IO ()
main = hakyll $ do

    route   "favicon.ico" idRoute
    compile "favicon.ico" copyFileCompiler

    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    route   "data/*" idRoute
    compile "data/*" copyFileCompiler

    route   "images/*" idRoute
    compile "images/*" copyFileCompiler

    route   "posts/*" $ setExtension "html"
    compile "posts/*" $ pageCompiler
        >>> arr (renderDateFieldWith cs "date" "%-d. %B %Y" "Neznámé datum")
        >>> renderTagsField "prettytags" (fromCaptureString "tags/*")
        >>> applyTemplateCompiler "templates/post.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    route  "posts.html" idRoute
    create "posts.html" $
        constA mempty
            >>> arr (setField "title" "Všechny texty")
            >>> requireAllA "posts/*" addPostList
            >>> applyTemplateCompiler "templates/posts.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    route  "index.html" idRoute
    create "index.html" $
        constA mempty
            >>> arr (setField "title" "Index of ~xsedlar3")
            >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
            >>> requireAllA "posts/*" (second (arr $ newest 5) >>> addPostList)
            >>> applyTemplateCompiler "templates/index.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    route   "tags/*" $ customRoute tagToRoute
    metaCompile $ require_ "tags"
        >>> arr (M.toList . tagsMap)
        >>> arr (map (\(t,p) -> (tagIdentifier t, makeTagList t p >>> relativizeUrlsCompiler)))

    compile "templates/*" templateCompiler

    route   "rss.xml" idRoute
    create "rss.xml" $
        requireAll_ "posts/*" >>> renderRss feedConfiguration

    return ()

  where
    newest :: Int -> [Page a] -> [Page a]
    newest n = take n . reverse . sortByBaseName

    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 200

    tagIdentifier :: String -> Identifier
    tagIdentifier = fromCaptureString "tags/*"

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
tagToRoute = (++".html") . stripDiacritics . intercalate "/" . unIdentifier

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
