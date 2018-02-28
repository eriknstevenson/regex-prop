{-# LANGUAGE QuasiQuotes #-}

module RegexSpec (spec) where

import Test.Hspec
import Text.Regex.PCRE.Heavy

import Test.Regex

spec :: Spec
spec = do

  {-
  -}
  describe "Equivalency" $ do

    let alphabet = alphabetOf "01"
        regexA = [re|^(0|1)*$|]
        regexB = [re|^(0|1)*(0|1)*$|]

    checkRegex "test" alphabet $
      regexA `isEquivalentTo` regexB

  {-
  -}
  describe "Example 1" $ do
    
    let alphabet = alphabetOf "01"
        regex = [re|^(0|(10))*$|]
    
    checkRegex "All 1s are immediately followed by a 0." alphabet $
      regex `satisfies` \w ->
        case length w of 
          0 -> True
          1 -> w /= "1"
          _ -> w `doesNotContain` "11" && w `doesNotEndWith` "1"
      
      
  {-
  -}
  describe "Example 2 - Equivalency" $ do

    let alphabet = alphabetOf "ab"
        regexA = [re|^(a|b)*b*$|]
        regexB = [re|^(a*|ba*)*$|]
        regexC = [re|^(b*(a|\z)b*)*$|]

    checkRegexMulti "All of the regexes are equivalent" alphabet 
      [ allEquivalent [regexA, regexB, regexC] 
      , allSatisfy [regexA, regexB, regexC] $ const True
      ]

  {-
  -}
  describe "Example 3" $ do

    let alphabet = alphabetOf "ab"
        regex = [re|^b*(a|bb+)*b*$|]

    checkRegex "Word does not have aba as substring" alphabet $
      regex `satisfies` \w ->
        w `doesNotContain` "aba"
      

  {-
  -}
  describe "Example 4" $ do

    let alphabet = alphabetOf "ab"
        regex = [re|^(a|ba|bba)*(b|bb|\z)$|]

    checkRegex "Word does not contain more than 2 consecutive bs" alphabet $
      regex `satisfies` \w ->
        w `doesNotContain` "bbb"
