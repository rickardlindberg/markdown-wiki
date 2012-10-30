module MarkdownWiki where

import Control.Monad
import qualified Data.Map as M
import System.Directory
import System.Environment
import System.FilePath
import Text.Pandoc
import Text.Regex.Posix

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

isWikiName :: String -> Bool
isWikiName string = string =~ ("^" ++ wikiNameRegex ++ "$")

wikiNameRegex = "\\b([A-Z][a-z]+)([A-Z][a-z]+)+\\b"

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
toLink = map toInline . splitWikiWord
    where
        toInline (True, x)  = Link [Str x] (x ++ ".html", x)
        toInline (False, x) = Str x

splitWikiWord :: String -> [(Bool, String)]
splitWikiWord str =
    case str =~ wikiNameRegex of
        (x, "", "")           -> [(False, x)]
        (before, match, rest) -> [(False, before), (True, match)] ++ splitWikiWord rest

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
    , "  </head>"
    , "  <body>"
    , "    <div class=\"container noshowgrid\">"
    , "      <div class=\"span-20 prepend-2 append-2\">"
    , "        <h1>" ++ name ++ "</h1>"
    , html
    , "      </div>"
    , "    </div>"
    , "  </body>"
    , "</html>"
    ]

main :: IO ()
main = do
    [inDir, outDir] <- getArgs
    readPages inDir >>= writePages outDir
