module Query where

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
    let vertices = expandTree name pages
        edges    = findLinks (map snd vertices) pages
    in Graph vertices edges

expandTree :: WikiName -> Pages -> [(Int, WikiName)]
expandTree name pages =
    let level1 = [name]
        level2 = filter (\x -> x `notElem` level1) $ nextLevel level1 pages
        level3 = filter (\x -> x `notElem` level1 && x `notElem` level2) $ nextLevel level2 pages
        l1     = map (\x -> (1, x)) level1
        l2     = map (\x -> (2, x)) level2
        l3     = map (\x -> (3, x)) level3
    in l1 ++ l2 ++ l3

findLinks :: [WikiName] -> Pages -> [(WikiName, WikiName)]
findLinks names pages = concatMap findLinksFrom names
    where
        findLinksFrom from = map (\to -> (from, to)) (linkedPages from)
        linkedPages name = filter (`elem` names) (findPagesLinkedFrom name pages)

nextLevel :: [WikiName] -> Pages -> [WikiName]
nextLevel names pages =
    let to   = concatMap (`findPagesLinkingTo` pages) names
        from = concatMap (`findPagesLinkedFrom` pages) names
    in uniqList $ to ++ from
    where
        uniqList :: Ord a => [a] -> [a]
        uniqList = S.toList . S.fromList

findPagesLinkingTo :: WikiName -> Pages -> [WikiName]
findPagesLinkingTo name pages = M.keys $ M.filter (hasLinkTo name) pages
    where
        hasLinkTo name page = name `elem` extractWikiLinks page

findPagesLinkedFrom :: WikiName -> Pages -> [WikiName]
findPagesLinkedFrom name pages =
    case M.lookup name pages of
        Nothing   -> []
        Just page -> extractWikiLinks page

extractWikiLinks :: Pandoc -> [WikiName]
extractWikiLinks = queryWith findWikiLinks
    where
        findWikiLinks :: Inline -> [WikiName]
        findWikiLinks (Str str) = map snd $ filter fst $ splitOnWikiNames str
        findWikiLinks _         = []
