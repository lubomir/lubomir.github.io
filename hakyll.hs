{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Char (toLower)
import Data.Monoid
import Prelude hiding (id)

import Hakyll
import Text.Pandoc
import Czech

main :: IO ()
main = hakyll $ do

    match "css/*" $ do
        route   $ setExtension "css"
        let sass = unixFilter "sass" ["-s", "-C", "-t", "compressed", "--scss"]
        compile $ getResourceString >>= withItemBody sass

    match "static/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/static.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match ("favicon.ico" .||. "data/*" .||. "images/*") $ do
        route   idRoute
        compile copyFileCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ (pandocCompilerWith defaultHakyllReaderOptions myWriterOptions)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["posts.html"] $ do
        route  idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" "Všechny texty" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Příspěvky označené jako " ++ tag

        -- FIXME Copied from posts
        route   $ customRoute tagToRoute `composeRoutes` setExtension "html"
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls


    match "index.html" $ do
        route  idRoute
        compile $ do
            list <- postList tags "posts/*" $ fmap (take 5) . recentFirst
            let indexContent = constField "posts" list `mappend`
                    field "tags" (\_ -> renderTagCloud' tags) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContent
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    forM_ ["403.html", "404.html"] $ \p ->
        match p $ do
            route   idRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile $ templateCompiler

    create ["rss.xml"] $ do
        route  idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration) feedCtx

    return ()

  where
    renderTagCloud' :: Tags -> Compiler String
    renderTagCloud' tags =
        let sorted = sortTagsBy caseInsensitiveTags tags
        in renderTagCloud 100 200 sorted

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions
    { writerHtml5 = True
    }

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , czechDateField "date"
    , tagsField "tags" tags
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts

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
