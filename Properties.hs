module Properties where

import MarkdownWiki
import Test.QuickCheck
import Text.Pandoc

wikiWord :: Gen String
wikiWord = do
    first <- elements ['A'..'Z']
    rest <- lowercase
    return $ first : rest

wikiName :: Gen String
wikiName = do
    first <- wikiWord
    rest <- listOf1 wikiWord
    return $ concat $ first : rest

lowercase :: Gen String
lowercase = listOf1 $ elements ['a'..'z']

prop_findsWikiNameAnywhere =
    forAll lowercase $ \before ->
    forAll wikiName  $ \name   ->
    forAll lowercase $ \after  ->
    let beforeSpaced = before ++ " "
        afterSpaced  = " " ++ after
    in splitWikiWord (beforeSpaced ++ name ++ afterSpaced) ==
        [ (False, beforeSpaced)
        , (True,  name)
        , (False, afterSpaced)
        ]

main :: IO ()
main = do
    quickCheck prop_findsWikiNameAnywhere
