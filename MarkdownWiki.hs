module MarkdownWiki where

import Process
import qualified Data.Map as M
import Query
import System.Directory
import System.Environment
import System.FilePath
import Text.Pandoc
import Text.StringTemplate
import Types
import WikiName

main :: IO ()
main = do
    [templatePath, inDir, outDir] <- getArgs
    readPages inDir >>= writePages templatePath outDir

readPages :: FilePath -> IO Pages
readPages dir = do
    fileNames <- getDirectoryContents dir
    let wikiFiles = filter isWikiName fileNames
    let wikiPaths = map (dir </>) wikiFiles
    contents <- mapM readFile wikiPaths
    let soyWikiProcessed = map (unlines . drop 2 . lines) contents
    let pandocs = map (readMarkdown defaultParserState) soyWikiProcessed
    return $ M.fromList (zip wikiFiles pandocs)

writePages :: FilePath -> FilePath -> Pages -> IO ()
writePages templatePath dir pages = do
    templateString <- readFile templatePath
    mapM_ (writePage (newSTMP templateString)) (M.assocs pages)
    where
        writePage st (name, pandoc) = do
            putStrLn $ "Writing " ++ name
            let html = writeHtmlString defaultWriterOptions (process root pandoc)
            let templified = templify st name html linkWeb
            writeFile (dir </> name ++ ".html") templified
        linkWeb = linkWebFrom pages

templify :: StringTemplate String -> WikiName -> String -> LinkWeb -> String
templify st name html linkWeb =
    render $ ( setAttribute "root" root
             . setAttribute "html" html
             . setAttribute "name" name
             . (\st -> foldr (setAttribute "vertices") st vertices)
             . (\st -> foldr (setAttribute "edges") st edges)
             ) st
    where
        (Graph vertices edges) = graphFrom name linkWeb

root = "."
