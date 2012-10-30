module WikiName where

import Text.Regex.Posix

wikiNameRegex = "\\b([A-Z][a-z]+)([A-Z][a-z]+)+\\b"

isWikiName :: String -> Bool
isWikiName string = string =~ ("^" ++ wikiNameRegex ++ "$")

splitOnWikiNames :: String -> [(Bool, String)]
splitOnWikiNames str =
    case str =~ wikiNameRegex of
        (x, "", "")           -> [(False, x)]
        (before, match, rest) -> [(False, before), (True, match)] ++ splitOnWikiNames rest
