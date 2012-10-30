module MarkdownWiki where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M
import System.Directory
import System.Environment
import System.FilePath
import Text.Pandoc
import WikiName

type Pages = M.Map WikiName Pandoc

type WikiName = String

readPages :: FilePath -> IO Pages
readPages dir = do
    fileNames <- getDirectoryContents dir
    let wikiFiles = filter isWikiName fileNames
    let wikiPaths = map (dir </>) wikiFiles
    contents <- mapM readFile wikiPaths
    let pandocs = map (readMarkdown defaultParserState) contents
    return $ M.fromList (zip wikiFiles pandocs)

writePages :: FilePath -> Pages -> IO ()
writePages dir pages = mapM_ writePage (M.assocs pages)
    where
        writePage (name, pandoc) = do
            let processed = bottomUp linkify pandoc
            let html = writeHtmlString defaultWriterOptions processed
            let templified = templify name html pages
            writeFile (dir </> name ++ ".html") templified

linkify :: [Inline] -> [Inline]
linkify []             = []
linkify ((Str str):xs) = toLink str ++ linkify xs
linkify (x:xs)         = x           : linkify xs

toLink :: String -> [Inline]
toLink = map toInline . splitOnWikiNames
    where
        toInline (True, x)  = Link [Str x] (x ++ ".html", x)
        toInline (False, x) = Str x

getRoot :: String -> String
getRoot name = "./" ++ name

templify :: WikiName -> String -> Pages -> String
templify name html pages = unlines
    [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\""
    , "\"http://www.w3.org/TR/html4/strict.dtd\">"
    , "<html>"
    , "  <head>"
    , "    <title>" ++ name ++ "</title>"
    , "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
    , "    <link rel=\"stylesheet\" href=\"" ++ getRoot "static" ++ "/css/blueprint/screen.css\" type=\"text/css\" media=\"screen, projection\">"
    , "    <link rel=\"stylesheet\" href=\"" ++ getRoot "static" ++ "/css/blueprint/print.css\" type=\"text/css\" media=\"print\"> "
    , "    <!--[if lt IE 8]>"
    , "      <link rel=\"stylesheet\" href=\"" ++ getRoot "static" ++ "/css/blueprint/ie.css\" type=\"text/css\" media=\"screen, projection\">"
    , "    <![endif]-->"
    , "    <link rel=\"stylesheet\" href=\"" ++ getRoot "static" ++ "/css/pygments/syntax.css\" type=\"text/css\"> "
    , "    <link rel=\"stylesheet\" href=\"" ++ getRoot "static" ++ "/css/layout.css\" type=\"text/css\"> "
    , "    <link rel=\"stylesheet\" href=\"" ++ getRoot "static" ++ "/css/colors.css\" type=\"text/css\"> "
    , "    <script type=\"text/javascript\" src=\"" ++ getRoot "static/js" ++ "/graph.js\"></script>"
    , "  </head>"
    , "  <body>"
    , "    <div class=\"container noshowgrid\">"
    , "      <div class=\"span-20 prepend-2 append-2\">"
    , "        <h1>" ++ name ++ "</h1>"
    , html
    , "        <hr />"
    , nav name pages
    , "      </div>"
    , "    </div>"
    , "  </body>"
    , "</html>"
    ]

linkReach :: WikiName -> Pages -> [[WikiName]]
linkReach name pages = [name] : [delete name (findLinksFrom name pages)]

findLinksFrom :: WikiName -> Pages -> [WikiName]
findLinksFrom name pages =
    case M.lookup name pages of
        Nothing   -> []
        Just page -> queryWith findWikiLinks page

findWikiLinks :: Inline -> [WikiName]
findWikiLinks (Str str) = map snd $ filter fst $ splitOnWikiNames str
findWikiLinks _         = []

nav :: WikiName -> Pages -> String
nav name pages = unlines
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" id=\"canvas\"></svg>"
    , "<script type=\"text/javascript\">"
    --, "<![CDATA["
    , "    var g = new Graph(\"canvas\", 800, 300);"
    , "    // reduce repulsion and spring length for more compact layout"
    --, "    g.repulsion = g.repulsion / 8;"
    --, "    g.spring_length = 1;"
    , vertices $ linkReach name pages
    , edges (concat $ linkReach name pages) pages
    , "    g.go();"
    --, "]]>"
    , "</script>"
    ]

vertices :: [[WikiName]] -> String
vertices levels = concatMap foo (zip ["#ff0000", "#00ff00", "#0000ff"] levels)
    where
        foo (color, names) = concatMap (\x -> "g.createVertex(\"" ++ x ++ "\", \"" ++ color ++ "\");\n") names

edges :: [WikiName] -> Pages -> String
edges names pages = concatMap foo names
    where
        foo name = concatMap (\x -> "g.createEdge(\"" ++ name ++ "\", \"" ++ x ++ "\");\n") (findLinksFrom name pages)

main :: IO ()
main = do
    [inDir, outDir] <- getArgs
    readPages inDir >>= writePages outDir
