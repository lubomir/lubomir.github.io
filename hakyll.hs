{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
                             , homeLink :: String
                             , topLink :: String
                             , contactLink :: String
                             , licensedAs :: String
                             }

mkContext :: BlogConfig -> Context String
mkContext (BlogConfig {..}) =
    mconcat [ constField "taggedAs" taggedAs
            , constField "backToMain" backToMain
            , constField "listHeader" listHeader
            , constField "tagsHeader" tagsHeader
            , dateFormatter "date"
            , constField "homeLink" homeLink
            , constField "topLink" topLink
            , constField "contactLink" contactLink
            , constField "licensedAs" licensedAs
            , defaultContext
            ]

czechConfig :: BlogConfig
czechConfig = BlogConfig "cs"
                         "Všechny texty"
                         "Příspěvky označené jako "
                         "posts/cs/*"
                         czechDateField
                         "zpět na hlavní stránku"
                         "Označeno jako"
                         "Domů"
                         "Nahoru"
                         "Kontakt"
                         "Licencováno jako Creative Commons Attribution"

englishConfig :: BlogConfig
englishConfig = BlogConfig "en"
                           "All posts"
                           "Posts tagged "
                           "posts/en/*"
                           (`dateField` "%B %-d, %Y")
                           "back to main page"
                           "Tagged as"
                           "Home"
                           "Back to top"
                           "Contact me"
                           "Licensed under Creative Commons Attribution"

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
            >>= defaultCompiler bc

    create [fromFilePath $ langPrefix </> "posts.html"] $ do
        route  idRoute
        compile $ do
            list <- postList bc tags postPattern recentFirst
            makeItem "" >>= postListCompiler bc listHeader list "posts"

    tagsRules tags $ \tag pattern -> do
        let title = tagsHeader ++ tag
        let fConf = feedConfiguration {
            feedTitle = feedTitle feedConfiguration ++ " – " ++ title
        }
        let fRoute = tagToRoute' tag

        route $ customRoute tagToRoute `composeRoutes` setExtension "html"
        compile $ do
            list <- postList bc tags pattern recentFirst
            makeItem "" >>= postListCompiler bc title list fRoute

        version "atom" $ do
            route $ customRoute tagToRoute `composeRoutes` setExtension "atom"
            compile $ loadAllSnapshots pattern "content"
                    >>= fmap (take 10) . recentFirst
                    >>= renderAtom fConf feedCtx

    create [fromFilePath $ langPrefix </> "posts.atom"] $ do
        route  idRoute
        compile $ loadAllSnapshots postPattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom feedConfiguration feedCtx

    return tags

getRecentPosts :: BlogConfig -> Tags -> Compiler String
getRecentPosts bc tags =
    postList bc tags (postPattern bc) $ fmap (take 5) . recentFirst

main :: IO ()
main = hakyll $ do

    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

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
            let indexContext = mconcat [ constField "csposts" cslist
                                       , constField "enposts" enlist
                                       , field "cstags" (\_ -> renderTagCloud' cstags)
                                       , field "entags" (\_ -> renderTagCloud' entags)
                                       , mkContext englishConfig
                                       ]
            getResourceBody >>= applyAsTemplate indexContext
                            >>= defaultCompiler englishConfig

    forM_ ["403.html", "404.html"] $ \p ->
        match p $ do
            route   idRoute
            compile $ myCompiler
                >>= loadAndApplyTemplate "templates/default.html" (mkContext englishConfig)

    match "templates/*" $ compile templateCompiler

    return ()

  where
    renderTagCloud' :: Tags -> Compiler String
    renderTagCloud' tags =
        renderTagCloud 100 200 (sortTagsBy caseInsensitiveTags tags)

postListCompiler :: BlogConfig
                 -> String
                 -> String
                 -> String
                 -> Item String
                 -> Compiler (Item String)
postListCompiler bc title list feed =
    loadAndApplyTemplate "templates/posts.html" (mconcat
        [ constField "title" title
        , constField "posts" list
        , defaultContext
        , constField "feed" feed
        , constField "backToMain" (backToMain bc)])
    >=> defaultCompiler bc


defaultCompiler :: BlogConfig -> Item String -> Compiler (Item String)
defaultCompiler bc = loadAndApplyTemplate "templates/default.html" (mkContext bc)
    >=> relativizeUrls

myCompiler :: Compiler (Item String)
myCompiler = pandocCompilerWithTransform def myWriterOptions czechPandocTransform
  where
    myWriterOptions = def { writerHtml5 = True
                          , writerHighlight = True
                          }

postCtx :: BlogConfig -> Tags -> Context String
postCtx bc tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , tagsField "tags" tags
    , mkContext bc
    ]

feedCtx :: Context String
feedCtx = bodyField "description" <> defaultContext

postList :: BlogConfig -> Tags -> Pattern -> ([Item String]
         -> Compiler [Item String]) -> Compiler String
postList bc tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (postCtx bc tags) posts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedRoot        = "http://lubomir.github.io/"
    , feedTitle       = "lsedlar"
    , feedDescription = "home of lsedlar"
    , feedAuthorName  = "Lubomír Sedlář"
    , feedAuthorEmail = "lubomir.sedlar@gmail.com"
    }

tagToRoute :: Identifier -> FilePath
tagToRoute = tagToRoute' . toFilePath

tagToRoute' :: String -> String
tagToRoute' = stripSpaces . stripDiacritics . map toLower
  where stripSpaces = map (\c -> if c == ' ' then '-' else c)
