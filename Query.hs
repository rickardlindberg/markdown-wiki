module Query where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Pandoc
import Types
import WikiName

data Graph = Graph
    { vertices :: [(Int, WikiName)]
    , edges    :: [(WikiName, WikiName)]
    }

graphFrom :: WikiName -> Pages -> Graph
graphFrom name pages =
    let from     = addLevel $ foo findLinksFrom name pages
        to       = addLevel $ foo findLinksTo name pages
        vertices = uniqList $ from ++ to
        allNames = map snd vertices
        edges    = concatMap (\name -> map (\to -> (name, to)) (filter (`elem` allNames) (findLinksFrom name pages))) allNames
    in Graph vertices edges
    where
        uniqList :: Ord a => [a] -> [a]
        uniqList = S.toList . S.fromList

        addLevel :: [[WikiName]] -> [(Int, WikiName)]
        addLevel links = concatMap (\(level, list) -> map (\e -> (level, e)) list) $ zip [1..] links

foo fn name pages =
    let level1 = [name]
        level2 = foldr delete (fn name pages) level1
        level3 = foldr delete (concatMap (`fn` pages) level2) (name:level2)
    in [level1, level2, level3]

findLinksTo :: WikiName -> Pages -> [WikiName]
findLinksTo name pages = M.keys $ M.filter (hasLinkTo name) pages
    where
        hasLinkTo name page = name `elem` extractWikiLinks page

findLinksFrom :: WikiName -> Pages -> [WikiName]
findLinksFrom name pages =
    case M.lookup name pages of
        Nothing   -> []
        Just page -> extractWikiLinks page

extractWikiLinks :: Pandoc -> [WikiName]
extractWikiLinks = queryWith findWikiLinks
    where
        findWikiLinks :: Inline -> [WikiName]
        findWikiLinks (Str str) = map snd $ filter fst $ splitOnWikiNames str
        findWikiLinks _         = []
