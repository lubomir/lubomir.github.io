import Text.Hakyll
import Control.Monad
import Data.List
import Data.Ord
import qualified Data.Map as M

main = hakyll "http://example.com" $ do
    directory css    "css"
    directory static "data"

    pagesPaths <- getRecursiveContents "pages"
    let pagesP = map createPage pagesPaths

    forM_ pagesP $ renderChain [ "templates/page.html"
                               , "templates/default.html"
                               ]
    let index = createListing "index.html"
                              [ "templates/pageitem.html"]
                              pagesP
                              [("title", Left "Home")]

    renderChain ["index.html", "templates/default.html"] index
