import Text.Hakyll
import Control.Monad
import Data.List
import Data.Ord
import qualified Data.Map as M

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
