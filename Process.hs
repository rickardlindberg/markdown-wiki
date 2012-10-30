module Process where

import Text.Pandoc
import WikiName

process :: Pandoc -> Pandoc
process = bottomUp linkify

linkify :: [Inline] -> [Inline]
linkify []           = []
linkify (Str str:xs) = toLink str ++ linkify xs
linkify (x:xs)       = x           : linkify xs

toLink :: String -> [Inline]
toLink = map toInline . splitOnWikiNames
    where
        toInline (True, x)  = Link [Str x] (x ++ ".html", x)
        toInline (False, x) = Str x
