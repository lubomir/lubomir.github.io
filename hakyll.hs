{-# LANGUAGE OverloadedStrings, RecordWildCards, ExistentialQuantification #-}

import Control.Monad (forM_, (>=>))
import Data.Char (toLower)
import Data.Monoid
import Prelude
import System.FilePath

import Hakyll
import Text.Pandoc
import Czech

data BlogConfig = BlogConfig { langPrefix :: String
                             , listHeader :: String
                             , tagsHeader :: String
                             , postPattern :: Pattern
                             , dateFormatter :: String -> Context String
                             , backToMain :: String
                             , taggedAs :: String
                             }

czechConfig :: BlogConfig
czechConfig = BlogConfig "cs"
                         "Všechny texty"
                         "Příspěvky označené jako "
                         "posts/cs/*"
                         czechDateField
                         "zpět na hlavní stránku"
                         "Označeno jako"

englishConfig :: BlogConfig
englishConfig = BlogConfig "en"
                           "All posts"
                           "Posts tagged "
                           "posts/en/*"
                           (`dateField` "%B %-d, %Y")
                           "back to main page"
                           "Tagged as"

postRoute :: BlogConfig -> Routes
postRoute (BlogConfig { langPrefix = lp }) = customRoute $
    (`replaceExtension` "html") . (lp </>) . takeFileName . toFilePath

subsite :: BlogConfig -> Rules Tags
subsite bc@(BlogConfig {..}) = do
    tags <- buildTags postPattern (fromCapture $ fromGlob $ langPrefix </> "tags/*.html")

    match postPattern $ do
        route   $ postRoute bc
        compile $ myCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate (fromFilePath "templates/post.html")
                                     (postCtx bc tags)
            >>= defaultCompiler

    create [fromFilePath $ langPrefix </> "posts.html"] $ do
        route  idRoute
        compile $ do
            list <- postList bc tags postPattern recentFirst
            makeItem "" >>= postListCompiler listHeader list

    tagsRules tags $ \tag pattern -> do
        let title = tagsHeader ++ tag
        route $ customRoute tagToRoute `composeRoutes` setExtension "html"
        compile $ do
            list <- postList bc tags pattern recentFirst
            makeItem "" >>= postListCompiler title list

    create [fromFilePath $ langPrefix </> "rss.xml"] $ do
        route  idRoute
        compile $
            loadAllSnapshots postPattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom feedConfiguration feedCtx

    return tags

getRecentPosts :: BlogConfig -> Tags -> Compiler String
getRecentPosts bc tags =
    postList bc tags (postPattern bc) $ fmap (take 5) . recentFirst

main :: IO ()
main = hakyll $ do

    match "css/*.scss" $ do
        route   $ setExtension "css"
        let sass = unixFilter "sass" ["-s", "-C", "-t", "compressed", "--scss"]
        compile $ getResourceString >>= withItemBody sass

    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match "static/*" $ do
        route   $ setExtension "html"
        compile $ myCompiler
            >>= loadAndApplyTemplate "templates/static.html" defaultContext
            >>= defaultCompiler

    match ("favicon.ico" .||. "data/*" .||. "images/**") $ do
        route   idRoute
        compile copyFileCompiler

    cstags <- subsite czechConfig
    entags <- subsite englishConfig

    match "index.html" $ do
        route  idRoute
        compile $ do
            cslist <- getRecentPosts czechConfig cstags
            enlist <- getRecentPosts englishConfig entags
            let indexContext = constField "csposts" cslist `mappend`
                    constField "enposts" enlist `mappend`
                    field "cstags" (\_ -> renderTagCloud' cstags) `mappend`
                    field "entags" (\_ -> renderTagCloud' entags) `mappend`
                    defaultContext
            getResourceBody >>= applyAsTemplate indexContext >>= defaultCompiler

    forM_ ["403.html", "404.html"] $ \p ->
        match p $ do
            route   idRoute
            compile $ myCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "templates/*" $ compile templateCompiler

    return ()

  where
    renderTagCloud' :: Tags -> Compiler String
    renderTagCloud' tags =
        renderTagCloud 100 200 (sortTagsBy caseInsensitiveTags tags)

postListCompiler :: String -> String -> Item String -> Compiler (Item String)
postListCompiler title list =
    loadAndApplyTemplate "templates/posts.html" (mconcat
        [ constField "title" title
        , constField "posts" list
        , defaultContext])
    >=> defaultCompiler


defaultCompiler :: Item String -> Compiler (Item String)
defaultCompiler = loadAndApplyTemplate "templates/default.html" defaultContext
    >=> relativizeUrls

myCompiler :: Compiler (Item String)
myCompiler = pandocCompilerWithTransform def myWriterOptions czechPandocTransform
  where
    myWriterOptions = def { writerHtml5 = True }

postCtx :: BlogConfig -> Tags -> Context String
postCtx bc tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateFormatter bc "date"
    , tagsField "tags" tags
    , constField "taggedAs" (taggedAs bc)
    , constField "backToMain" (backToMain bc)
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = bodyField "description" `mappend` defaultContext

postList :: BlogConfig -> Tags -> Pattern -> ([Item String]
         -> Compiler [Item String]) -> Compiler String
postList bc tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (postCtx bc tags) posts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedRoot        = "http://fi.muni.cz/~xsedlar3/"
    , feedTitle       = "~xsedlar3"
    , feedDescription = "home of ~xsedlar3"
    , feedAuthorName  = "Lubomír Sedlář"
    , feedAuthorEmail = "lsedlar@mail.muni.cz"
    }

tagToRoute :: Identifier -> FilePath
tagToRoute = stripSpaces . stripDiacritics . map toLower . toFilePath
  where stripSpaces = map (\c -> if c == ' ' then '-' else c)
