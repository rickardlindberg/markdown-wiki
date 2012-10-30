module Query where

import Data.List
import qualified Data.Map as M
import Text.Pandoc
import Types
import WikiName

linkReach :: WikiName -> Pages -> [[WikiName]]
linkReach name pages =
    let level1 = [name]
        level2 = foldr delete (findLinksFrom name pages) level1
        level3 = foldr delete (concatMap (\name -> findLinksFrom name pages) level2) (name:level2)
    in [level1, level2, level3]

findLinksFrom :: WikiName -> Pages -> [WikiName]
findLinksFrom name pages =
    case M.lookup name pages of
        Nothing   -> []
        Just page -> queryWith findWikiLinks page

findWikiLinks :: Inline -> [WikiName]
findWikiLinks (Str str) = map snd $ filter fst $ splitOnWikiNames str
findWikiLinks _         = []
