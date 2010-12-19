import Text.Hakyll
import Control.Monad

defaultTitle = "~xsedlar3"

main = hakyll "http://example.com" $ do
    directory css    "css"
    directory static "data"
    directory static "images"

    pagesPaths <- getRecursiveContents "pages"
    let pagesP = map createPage pagesPaths

    forM_ pagesP $ renderChain [ "templates/page.html"
                               , "templates/default.html"
                               ]
    let index = createListing "index.html"
                              [ "templates/pageitem.html"]
                              pagesP
                              [("title", Left defaultTitle)]

    renderChain ["index.html", "templates/default.html"] index
