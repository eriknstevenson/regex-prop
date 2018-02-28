module Test.Regex where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Regex.PCRE.Heavy


type Alphabet = Gen String

type RegexProperty = Alphabet -> Property

alphabetOf :: String -> Alphabet
alphabetOf alphabet = 
  listOf (elements alphabet)


checkRegex :: String -> Alphabet -> RegexProperty -> Spec
checkRegex desc alphabet regexProp = 
  prop desc $ regexProp alphabet


checkRegexMulti :: String -> Alphabet -> [RegexProperty] -> Spec
checkRegexMulti desc alphabet regexProps = 
  prop desc $ conjoin $ map (\f -> f alphabet) regexProps


satisfies :: Regex -> (String -> Bool) -> RegexProperty
satisfies regex predicate alphabet =
  forAll alphabet $ \w ->
    w =~ regex === predicate w


allSatisfy :: [Regex] -> (String -> Bool) -> RegexProperty
allSatisfy regex predicate alphabet =
  forAll alphabet $ \w ->
    conjoin $ map (\r -> w =~ r === predicate w) regex


isEquivalentTo :: Regex -> Regex -> RegexProperty
isEquivalentTo a b alphabet =
  forAll alphabet $ \w ->
    (w =~ a) === (w =~ b)


allEquivalent :: [Regex] -> RegexProperty
allEquivalent regexes alphabet = 
  forAll alphabet $ \w ->
    conjoin $ map (w =~) regexes

     
doesNotContain :: Eq a => [a] -> [a] -> Bool
doesNotContain s x = 
  not (x `isInfixOf` s)


doesNotEndWith :: Eq a => [a] -> [a] -> Bool
doesNotEndWith s x = 
  not (x `isSuffixOf` s)
