{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RegexSpec (spec) where

import Data.Functor
import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.ParserCombinators.ReadP
import Text.Regex.PCRE.Heavy

import Test.Regex

spec :: Spec
spec = do

  describe "QuickCheck demo" $
    prop "Reversing a list twice produces the original list" $ \(xs :: [Int]) ->
      reverse (reverse xs) === xs

-----------------------------------------------------------

  describe "Verifying regex matches a language" $ do
    
    describe "Example 1" $ do

      let alphabet = alphabetOf "ab"
          regex = [re|^b*(a|bb+)*b*$|]

      checkRegex "Word does not have 'aba' as substring" alphabet $
        regex `satisfies` \w ->
          w `doesNotContain` "aba"


    describe "Example 2" $ do

      let alphabet = alphabetOf "ab"
          regex = [re|^(a|ba|bba)*(b|bb|\z)$|]

      checkRegex "Word does not contain more than 2 consecutive bs" alphabet $
        regex `satisfies` \w ->
          w `doesNotContain` "bbb"
    

    describe "Example 3" $ do
      let alphabet = alphabetOf "01"
          regex = [re|^(0|(10))*$|]
      
      checkRegex "All 1s are immediately followed by a 0." alphabet $
        regex `satisfies` \w ->
          case length w of 
            0 -> True
            1 -> w /= "1"
            _ -> w `doesNotContain` "11" && w `doesNotEndWith` "1"


    describe "Example 4" $ do

      let alphabet = alphabetOf "ab"
          regex = [re|^((b*ab*(b*ab*ab*)*b*)|(a*ba*(a*ba*ba*)*a*))$|]

      checkRegex "Word contains odd number of As or odd number of Bs" alphabet $
        regex `satisfies` \w ->
          let occurrences = map length . group . sort $ w
          in any odd occurrences

-----------------------------------------------------------

  describe "Equivalency of regular expressions" $ do

    describe "Example 1" $ do
      let alphabet = alphabetOf "01"
          regexA = [re|^(01|0)*0$|]
          regexB = [re|^0(10|0)*$|]

      checkRegex "(01|0)*0 is equivalent to 0(10|0)*" alphabet $
        regexA `isEquivalentTo` regexB
          

    describe "Example 2 - Verifying 3 regular expressions are all equivalent" $ do

      let alphabet = alphabetOf "ab"
          regexA = [re|^(a|b)*b*$|]
          regexB = [re|^(a*|ba*)*$|]
          regexC = [re|^(b*(a|\z)b*)*$|]

      checkRegex "All of the regexes are equivalent" alphabet $
        allEquivalent [regexA, regexB, regexC] 


    describe "Example 3" $ do

      let alphabet = alphabetOf "ab"
          regexA = [re|^(ba)+(a*b*|a*)$|]
          regexB = [re|^(ba)*ba+(b*|\z)$|]

      checkRegex "(ba)+(a*b*|a*) is equivalent to (ba)*ba+(b*|\\z)" alphabet $
        regexA `isEquivalentTo` regexB

-----------------------------------------------------------

  describe "Verifying with parser combinator" $
      
      describe "Example 1" $ do

        let alphabet = alphabetOf "ab"
            regex = [re|^b(bb)*(aa)*$|]
            
            validator :: String -> Bool
            validator input = not (null (parser input))

            parser :: ReadS ()
            parser = readP_to_S $
              char 'b' 
              *> many (pairOf 'b') 
              *> many (pairOf 'a')
              *> eof
              $> ()

        checkRegex "A language of strings containing an even number of A's, an odd number of B's, and not containing the substring ab" alphabet $
          regex `satisfies` validator
      

pairOf :: Char -> ReadP String
pairOf c =
  string [c,c]
