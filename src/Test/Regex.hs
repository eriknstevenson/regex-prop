module Test.Regex where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Regex.PCRE.Heavy


-- |A source of strings containing symbols of a certain alphabet
type Alphabet = Gen String

-- |A Property which needs to be given an Alphabet to evaluate 
type RegexProperty = Alphabet -> Property

-- |Constructs an alphabet with the given characters.
-- Words produced by this alphabet will consist only of these characters.
alphabetOf :: String -> Alphabet
alphabetOf alphabet = 
  listOf (elements alphabet)

-- |Verify that a Regex matches strings with a given property.
checkRegex :: String -> Alphabet -> RegexProperty -> Spec
checkRegex desc alphabet regexProp = 
  prop desc $ regexProp alphabet

-- |Verify multiple Regex properties at the same time.
checkRegexMulti :: String -> Alphabet -> [RegexProperty] -> Spec
checkRegexMulti desc alphabet regexProps = 
  prop desc $ conjoin $ map (\f -> f alphabet) regexProps

-- |Make a claim that a Regex will match strings with a given property.
satisfies :: Regex -> (String -> Bool) -> RegexProperty
satisfies regex predicate alphabet =
  forAll alphabet $ \w ->
    w =~ regex === predicate w

-- |Make a claim that multiple Regexes will all match strings which satisfy a 
-- given property.
allSatisfy :: [Regex] -> (String -> Bool) -> RegexProperty
allSatisfy regex predicate alphabet =
  forAll alphabet $ \w ->
    conjoin $ map (\r -> w =~ r === predicate w) regex

-- |Assesses whether two Regexes act identically when provided same input 
-- strings.
isEquivalentTo :: Regex -> Regex -> RegexProperty
isEquivalentTo a b alphabet =
  forAll alphabet $ \w ->
    (w =~ a) === (w =~ b)

-- |Assesses whether multiple Regexes act identically when provided same input 
-- strings.
allEquivalent :: [Regex] -> RegexProperty
allEquivalent regexes alphabet = 
  forAll alphabet $ \w ->
    conjoin $ map (w =~) regexes

-- |Returns whether given string does not contain given substring.
doesNotContain :: Eq a => [a] -> [a] -> Bool
doesNotContain string substring = 
  not (substring `isInfixOf` string)

-- |Returns whether given string does not have given suffix.
doesNotEndWith :: Eq a => [a] -> [a] -> Bool
doesNotEndWith string suffix = 
  not (suffix `isSuffixOf` string)
