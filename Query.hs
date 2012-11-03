module Query where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Pandoc
import Types
import WikiName

type LinkWeb = M.Map WikiName [WikiName]

linkWebFrom :: Pages -> LinkWeb
linkWebFrom = M.map extractWikiLinks

data Graph = Graph
    { vertices :: [(Int, WikiName)]
    , edges    :: [(WikiName, WikiName)]
    }

graphFrom :: WikiName -> Pages -> Graph
graphFrom name pages =
    let linkWeb  = linkWebFrom pages
        vertices = expandTree name linkWeb
        edges    = findLinks (map snd vertices) linkWeb
    in Graph vertices edges

expandTree :: WikiName -> LinkWeb -> [(Int, WikiName)]
expandTree name linkWeb =
    let level1 = [name]
        level2 = filter (\x -> x `notElem` level1) $ nextLevel level1 linkWeb
        level3 = filter (\x -> x `notElem` level1 && x `notElem` level2) $ nextLevel level2 linkWeb
        l1     = map (\x -> (1, x)) level1
        l2     = map (\x -> (2, x)) level2
        l3     = map (\x -> (3, x)) level3
    in l1 ++ l2 ++ l3

findLinks :: [WikiName] -> LinkWeb -> [(WikiName, WikiName)]
findLinks names linkWeb = concatMap findLinksFrom names
    where
        findLinksFrom from = map (\to -> (from, to)) (linkedPages from)
        linkedPages from = filter (`elem` names) (M.findWithDefault [] from linkWeb)

nextLevel :: [WikiName] -> LinkWeb -> [WikiName]
nextLevel names linkWeb =
    let to   = concatMap (`findPagesLinkingTo` linkWeb) names
        from = concatMap (`findPagesLinkedFrom` linkWeb) names
    in uniqList $ to ++ from

findPagesLinkingTo :: WikiName -> LinkWeb -> [WikiName]
findPagesLinkingTo name linkWeb = M.keys $ M.filter (name `elem`) linkWeb

findPagesLinkedFrom :: WikiName -> LinkWeb -> [WikiName]
findPagesLinkedFrom name linkWeb = M.findWithDefault [] name linkWeb

extractWikiLinks :: Pandoc -> [WikiName]
extractWikiLinks = uniqList . queryWith findWikiLinks
    where
        findWikiLinks :: Inline -> [WikiName]
        findWikiLinks (Str str) = map snd $ filter fst $ splitOnWikiNames str
        findWikiLinks _         = []

uniqList :: Ord a => [a] -> [a]
uniqList = S.toList . S.fromList
