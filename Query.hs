module Query where

import Data.List
import qualified Data.Map as M
import Text.Pandoc
import Types
import WikiName

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
