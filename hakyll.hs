import Text.Hakyll
import Control.Monad
import Control.Arrow
import Data.List
import System.Locale
import Text.Pandoc (defaultWriterOptions, WriterOptions(..))

defaultTitle = "~xsedlar3"
webUrl = "http://fi.muni.cz/~xsedlar3"

myConfig = (defaultHakyllConfiguration webUrl) {
    pandocWriterOptions = defaultWriterOptions { writerSectionDivs = False }
  }

main = hakyllWithConfiguration myConfig $ do
    static           "favicon.ico"
    directory css    "css"
    directory static "data"
    directory static "images"

    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map ((>>> postManipulation) . createPage) postPaths

    let tagMap = readTagMap "postTags" postPaths

    renderPostList "posts.html" "Všechny texty" renderablePosts

    let renderListForTag tag posts =
            renderPostList (tagToUrl tag) ("Texty označené jako " ++ tag)
                           (map (>>> postManipulation) posts)
    withTagMap tagMap renderListForTag

    -- Render index, including recent posts.
    let tagCloud = tagMap >>> renderTagCloud tagToUrl 100 200
        index = createListing "index.html"
                              ["templates/postitem.html"]
                              (take 5 renderablePosts)
                              [ ("title", Left "~xsedlar3")
                              , ("tagcloud", Right tagCloud)
                              ]
    renderChain ["index.html", "templates/default.html"] index

    -- Render all posts.
    forM_ renderablePosts $ renderChain [ "templates/post.html"
                                        , "templates/default.html"
                                        ]

    -- Render rss feed
    renderRss myFeedConfiguration $
        map (>>> copyValue "body" "description") (take 5 renderablePosts)

  where
    postManipulation =   renderDateWithLocale cs "date" "%-d. %B %Y" "Neznámé datum"
                     >>> renderTagLinks tagToUrl

    tagToUrl tag = "$root/tags/" ++ (stripDiacritics . removeSpaces) tag ++ ".html"

    renderPostList url title posts = do
        let list = createListing url ["templates/postitem.html"]
                                 posts [("title", Left title)]
        renderChain ["posts.html", "templates/default.html"] list

myFeedConfiguration = FeedConfiguration
    { feedUrl         = "rss.xml"
    , feedTitle       = "~xsedlar3"
    , feedDescription = "home of ~xsedlar3"
    , feedAuthorName  = "Lubomír Sedlář"
    }

stripDiacritics :: String -> String
stripDiacritics str = foldl (\s (f,t) -> replace f t s) str diacritics

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace f r (x:xs)
  | f == x    = r : replace f r xs
  | otherwise = x : replace f r xs

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
