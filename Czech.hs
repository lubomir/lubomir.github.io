{-# LANGUAGE OverloadedStrings #-}
module Czech ( stripDiacritics
             , czechDateField
             , czechPandocTransform
             ) where

import Control.Arrow (first)
import Control.Applicative ((<$>), (<*>))
import Hakyll (dateFieldWith, Context(..))
import Data.Char (isAscii, toLower)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import System.Locale
import Text.Pandoc (Pandoc(..), Inline(..), topDown)
import Text.Parsec

-- |Remove accents from above letters.
stripDiacritics :: String -> String
stripDiacritics = T.unpack . T.filter isAscii . ICU.normalize ICU.NFD . T.pack

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

fixNumberOrRange :: String -> String
fixNumberOrRange s = case parse numberRange "" s of
    Left _ -> s
    Right n -> n

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

pass3 :: [Inline] -> [Inline]
pass3 [] = []
pass3 (Str s : xs) = Str (fixNumberOrRange s) : pass3 xs
pass3 (x:xs) = x : pass3 xs

-- | Helper filter that adds smart typography to Czech texts.
--
--   * adds nonbreakable spaces after single letter conjunctions
--     and prepositions
--
--   * adds thin nonbreakable spaces between number and unit
--
--   * converts - to – if surrounded by spaces or in number ranges
--
--   * converts ... to …
--
--   * creates proper Czech quotes
--
czechPandocTransform :: Pandoc -> Pandoc
czechPandocTransform = topDown (pass2 False . pass3 . pass1)
