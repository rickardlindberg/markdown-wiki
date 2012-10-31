module Process where

import Data.String.Utils
import Text.Pandoc
import WikiName

process :: String -> Pandoc -> Pandoc
process root = bottomUp (expandRoot root) . bottomUp linkify

linkify :: [Inline] -> [Inline]
linkify []           = []
linkify (Str str:xs) = toLink str ++ linkify xs
linkify (x:xs)       = x           : linkify xs

expandRoot :: String -> Inline -> Inline
expandRoot root (Link  x (url, y)) = Link  x (replace "$ROOT" root url, y)
expandRoot root (Image x (url, y)) = Image x (replace "$ROOT" root url, y)
expandRoot root x                  = x

toLink :: String -> [Inline]
toLink = map toInline . splitOnWikiNames
    where
        toInline (True, x)  = Link [Str x] (x ++ ".html", x)
        toInline (False, x) = Str x
