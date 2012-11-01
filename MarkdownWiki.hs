module MarkdownWiki where

import Process
import qualified Data.Map as M
import Query
import System.Directory
import System.Environment
import System.FilePath
import Text.Pandoc
import Types
import WikiName

readPages :: FilePath -> IO Pages
readPages dir = do
    fileNames <- getDirectoryContents dir
    let wikiFiles = filter isWikiName fileNames
    let wikiPaths = map (dir </>) wikiFiles
    contents <- mapM readFile wikiPaths
    let soyWikiProcessed = map (unlines . drop 2 . lines) contents
    let pandocs = map (readMarkdown defaultParserState) soyWikiProcessed
    return $ M.fromList (zip wikiFiles pandocs)

writePages :: FilePath -> Pages -> IO ()
writePages dir pages = mapM_ writePage (M.assocs pages)
    where
        writePage (name, pandoc) = do
            let html = writeHtmlString defaultWriterOptions (process (getRoot "") pandoc)
            let templified = templify name html pages
            writeFile (dir </> name ++ ".html") templified

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
    , "        <hr />"
    , "      </div>"
    , "    </div>"
    , "  </body>"
    , "</html>"
    ]

nav :: WikiName -> Pages -> String
nav name pages = unlines
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" id=\"canvas\"></svg>"
    , "<script type=\"text/javascript\">"
    --, "<![CDATA["
    , "    var g = new Graph(\"canvas\", 800, 400);"
    --, "    // reduce repulsion and spring length for more compact layout"
    , "    g.repulsion = g.repulsion / 2;"
    --, "    g.spring_length = 8;"
    , graph name pages
    , "    g.go();"
    --, "]]>"
    , "</script>"
    ]

graph :: WikiName -> Pages -> String
graph name pages =
    let (Graph vertices edges) = graphFrom name pages
        vList = map (\(level, name) -> "g.createVertex(\"" ++ name ++ "\", \"" ++ color level ++ "\", \"" ++ stroke level ++ "\", function () { location.href = \"" ++ name ++ ".html\"; });") vertices
        eList = map (\(from, to) -> "g.createEdge(\"" ++ from ++ "\", \"" ++ to ++ "\");") edges
    in unlines $ vList ++ eList
    where
        color  1 = "#fcaf3e"
        color  2 = "#fce94f"
        color  3 = "#8ae234"
        color  _ = "#cccccc"
        stroke 1 = "#ab7423"
        stroke 2 = "#c4a000"
        stroke 3 = "#425d26"
        stroke _ = "#000000"

main :: IO ()
main = do
    [inDir, outDir] <- getArgs
    readPages inDir >>= writePages outDir
