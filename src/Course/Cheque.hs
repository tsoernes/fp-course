{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Parser
import Course.MoreParser

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion =
  let preillion :: List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion :: List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

digits :: Char -> Optional Chars
digits '0' = Full "zero"
digits '1' = Full "one"
digits '2' = Full "two"
digits '3' = Full "three"
digits '4' = Full "four"
digits '5' = Full "five"
digits '6' = Full "six"
digits '7' = Full "seven"
digits '8' = Full "eight"
digits '9' = Full "nine"
digits _   = Empty

digitsNoZ :: Char -> Optional Chars
digitsNoZ '0' = Empty
digitsNoZ x   = digits x

digitsEmZ :: Char -> Optional Chars
digitsEmZ '0' = Full ""
digitsEmZ x   = digits x

teens :: Char -> Optional Chars
teens '0' = Full "ten"
teens '1' = Full "eleven"
teens '2' = Full "twelve"
teens '3' = Full "thirteen"
teens '4' = Full "fourteen"
teens '5' = Full "fifteen"
teens '6' = Full "sixteen"
teens '7' = Full "seventeen"
teens '8' = Full "eighteen"
teens '9' = Full "nineteen"
teens _    = Empty

tys :: Char -> Optional Chars
tys '2' = Full "twenty"
tys '3' = Full "thirty"
tys '4' = Full "forty"
tys '5' = Full "fifty"
tys '6' = Full "sixty"
tys '7' = Full "seventy"
tys '8' = Full "eighty"
tys '9' = Full "ninety"
tys _   = Empty

tens :: Char -> Optional Chars
tens '1' = Full "ten"
tens x   = tys x

oneOfChar :: (Char -> Optional Chars) -> Parser Chars
oneOfChar p = flbindParser character
  (\c -> case p c of
           Empty    -> unexpectedCharParser c
           Full str -> valueParser str)

-- Consume all input, if any, and succeed with given value
valueParserC :: a -> Parser a
valueParserC out = list (satisfy (const True)) >>> valueParser out


-- Parse 10 through 19
parseTeen :: Parser Chars
parseTeen = is '1' >>> oneOfChar teens

-- Parse 20 through 99
parseTys :: Parser Chars
parseTys = concatPsWith (oneOfChar tys) (oneOfChar digitsNoZ) "-"
       ||| concatPs (oneOfChar tys) (oneOfChar digitsEmZ)

-- Parse 01 through 99
parseTens :: Parser Chars
parseTens = parseTeen
        ||| parseTys
        ||| is '0' >>> oneOfChar digitsEmZ

parseHuns :: Parser Chars
parseHuns = concatPs (oneOfChar digitsNoZ) (valueParser " hundred")

-- Parse 000 through 999. 000 yields empty string.
-- Can this be made to work so that 1 yields one and 10 yiels ten
parseHundreds :: Parser Chars
parseHundreds = is '0' >>> parseTens
            ||| parseHuns <<< string "00"
            ||| concatPsWith parseHuns parseTens " and "
            ||| parseTens


-- Parse cents (0-99), fail with "zero"
-- fails for 00
parseCents :: Parser Chars
parseCents = parseTens ||| oneOfChar tens ||| valueParserC "zero"

-- Parse dollars, fail with "zero"
--parseDollars :: Chars -> Chars
--Issue: There's some funkyness when trying to 'show' the result from the parser.
--  Leading and trailing escaped quotes \"
--  Prolly something weird with custom list instance
parseDollars inp = map f illgroups
  where
    -- pad leading 0's so that all groups are of length 3
    mod' x y = if x == y then x else mod x y
    pad = replicate (3 - mod' (length inp) 3) '0' ++ inp
    -- group by 3
    groups = reverse $ groupN pad 3
    --- zip up with corresponding illion
    illgroups = reverse $ zip groups illion
    f :: (Chars, Chars) -> Chars
    f (str, illion') = (showRes $ parse parseHundreds str) ++ ' ' :.  illion'

groupN :: List a -> Int -> List (List a)
groupN xs n = groupN' xs
  where
    groupN' Nil = Nil
    groupN' xs' = (take n xs') :. (groupN' $ drop n xs')

-- Remove anything other than numbers and dots
-- Remove any dot besides the first
-- centsStr = dropWhile (/= '.') str
-- put "and" before amount in cents and after "hundred"
--
-- split into groups of three and three digits with corresponding illion
-- 1234567890
-- 1 204 067 890
-- [(001, "million"), (204, "thousand") (067, "hundred"), (890, "")]
-- map with a function that parseHundreds and concats
--
dollars' :: Chars -> Chars
dollars' str = cStr ++ "and " ++ cStr
  where
    str' = filter (\c -> isDigit c || c == '.') str
    (ds, r) = span (/= '.') str'
    cs = filter isDigit r
    --dStr = parseDollars ds
    cStr = listh $ show $ parse parseCents cs

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars = dollars'
