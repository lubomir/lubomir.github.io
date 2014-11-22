{-# LANGUAGE OverloadedStrings #-}
module Czech ( stripDiacritics
             , czechDateField
             , czechPandocTransform
             ) where

import Control.Arrow (first)
import Control.Applicative ((<$>), (<*>))
import Hakyll (dateFieldWith, Context(..))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Locale
import Text.Pandoc (Pandoc(..), Inline(..), topDown)
import Text.Parsec

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
                , intervals = []
                , amPm = ("am", "pm")
                , dateTimeFmt = ""
                , dateFmt = ""
                , timeFmt = ""
                , time12Fmt = ""
                }

conjuctions, dashes :: [String]
conjuctions = ["a", "i", "k", "o", "s", "u", "v", "z"]
dashes = ["-", "–", "—"]

isUnit :: String -> Bool
isUnit u = u `elem` ["g", "dg", "dag", "kg", "ml", "l"]

isNumberOrRange :: String -> Bool
isNumberOrRange s = case parse numberRange "" s of
    Left _  -> False
    Right _ -> True

numberRange, number, dash :: Monad m => ParsecT String () m String
numberRange = (++) <$> number <*> option "" ((++) <$> dash <*> number)
number = (++) <$> many1 digit <*> option "" ((:) <$> char ',' <*> many1 digit)
dash = choice (map string dashes) >> return "–"

isDash :: String -> Bool
isDash s = s `elem` dashes

nbsp, thinspace :: String
nbsp = " "
--nbsp = "×"
thinspace = " "
--thinspace = "÷"

pass1 :: [Inline] -> [Inline]
pass1 [] = []
pass1 (Str "..." : xs) = Str "…" : pass1 xs
pass1 (Space : Str s : Space : xs)
    | isDash s = Space : Str "–" : Space : pass1 xs
    | otherwise = Space : pass1 (Str s : Space : xs)
pass1 (Str s : Space : Str u : xs)
    | isNumberOrRange s && isUnit u = Str s : Str thinspace : pass1 (Str u : xs)
    | map toLower s `elem` conjuctions = Str s : Str nbsp : pass1 (Str u : xs)
    | otherwise = Str s : pass1 (Space : Str u : xs)
pass1 (Str s : Space : xs)
    | map toLower s `elem` conjuctions = Str s : Str nbsp : pass1 xs
    | otherwise = Str s : pass1 (Space : xs)
pass1 (x:xs) = x : pass1 xs

replaceQ :: Bool -> String -> (String, Bool)
replaceQ q [] = ([], q)
replaceQ q ('"' : xs) = first (quote++) $ replaceQ (not q) xs
  where quote = if q then "”" else "„"
replaceQ q (x : xs) = first (x:) $ replaceQ q xs

pass2 :: Bool -> [Inline] -> [Inline]
pass2 _ [] = []
pass2 q (Str s : xs) = let (s', q') = replaceQ q s
                       in Str s' : pass2 q' xs
pass2 q (x:xs) = x : pass2 q xs

-- | Helper filter that adds smart typography to Czech texts.
--
--   * adds nonbreakable spaces after single letter conjunctions
--     and prepositions
--
--   * adds thin nonbreakable spaces between number and unit
--
--   * converts - to – if surrounded by spaces
--
--   * converts ... to …
--
--   * creates proper Czech quotes
--
czechPandocTransform :: Pandoc -> Pandoc
czechPandocTransform = topDown (pass2 False . pass1)
