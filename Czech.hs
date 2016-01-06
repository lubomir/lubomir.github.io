{-# LANGUAGE OverloadedStrings #-}
module Czech ( stripDiacritics
             , czechDateField
             , czechPandocTransform
             ) where

import Hakyll (dateFieldWith, Context(..))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Time.Format
import Text.Pandoc (Pandoc(..), Inline(..), topDown)

-- |Remove accents from above letters.
stripDiacritics :: String -> String
stripDiacritics = map (\c -> fromMaybe c (lookup c pairs))
  where
    pairs = [('á', 'a'), ('č', 'c'), ('ď', 'd'), ('é', 'e'), ('ě', 'e'),
             ('í', 'i'), ('ó', 'o'), ('ň', 'n'), ('ř', 'r'), ('š', 's'),
             ('ť', 't'), ('ú', 'u'), ('ů', 'u'), ('ý', 'y'), ('ž', 'z'),
             ('ü', 'u'), ('ö', 'o'), ('ä', 'a'), ('ë', 'e'), ('ï', 'i'),
             ('â', 'a'), ('ê', 'e'), ('î', 'i'), ('ô', 'o'), ('û', 'u'),
             ('à', 'a'), ('è', 'e'), ('ì', 'i'), ('ò', 'o'), ('ù', 'u'),
             ('Á', 'A'), ('Č', 'C'), ('Ď', 'D'), ('É', 'E'), ('Ě', 'E'),
             ('Í', 'I'), ('Ó', 'O'), ('Ň', 'N'), ('Ř', 'R'), ('Š', 'S'),
             ('Ť', 'T'), ('Ú', 'U'), ('Ů', 'U'), ('Ý', 'Y'), ('Ž', 'Z'),
             ('Ü', 'U'), ('Ö', 'O'), ('Ä', 'A'), ('Ë', 'E'), ('Ï', 'I'),
             ('Â', 'A'), ('Ê', 'E'), ('Î', 'I'), ('Ô', 'O'), ('Û', 'U'),
             ('À', 'A'), ('È', 'E'), ('Ì', 'I'), ('Ò', 'O'), ('Ù', 'U')]

czechDateField :: String -> Context s
czechDateField field = dateFieldWith cs field "%-d. %B %Y"

cs :: TimeLocale
cs = TimeLocale { wDays = [ ("pondělí", "po"), ("úterý", "út"), ("středa", "st"),
                            ("čtvrtek", "čt"), ("pátek", "pá"), ("sobota", "so"),
                            ("neděle", "ne") ]
                , months = [
                    ("leden", "led"), ("únor", "ún"), ("březen", "bře"),
                    ("duben", "dub"), ("květen", "kvě"), ("červen", "čer"),
                    ("červenec", "čec"), ("srpen", "srp"), ("září", "žá"),
                    ("říjen", "ří"), ("listopad", "lis"), ("prosinec", "pro") ]
                , amPm = ("am", "pm")
                , dateTimeFmt = ""
                , dateFmt = ""
                , timeFmt = ""
                , time12Fmt = ""
                , knownTimeZones = []
                }

conjuctions :: [String]
conjuctions = ["a", "i", "k", "o", "s", "u", "v", "z"]

nbsp :: String
nbsp = " "

pass1 :: [Inline] -> [Inline]
pass1 [] = []
pass1 (Str s : Space : xs)
    | map toLower s `elem` conjuctions = Str s : Str nbsp : pass1 xs
    | otherwise = Str s : pass1 (Space : xs)
pass1 (x:xs) = x : pass1 xs

-- | Helper filter that adds smart typography to Czech texts.
--
--   * adds nonbreakable spaces after single letter conjunctions
--     and prepositions
--
czechPandocTransform :: Pandoc -> Pandoc
czechPandocTransform = topDown pass1
