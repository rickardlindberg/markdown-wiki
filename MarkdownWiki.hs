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

wikiNameRegex = "\\b([A-Z][a-z]+)+\\b"

writePages :: FilePath -> Pages -> IO ()
writePages dir pages = mapM_ writePage (M.assocs pages)
    where
        writePage (name, pandoc) = do
            let processed = bottomUp linkify pandoc
            let html = writeHtmlString defaultWriterOptions processed
            writeFile (dir </> name ++ ".html") html

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

main :: IO ()
main = do
    [inDir, outDir] <- getArgs
    readPages inDir >>= writePages outDir
