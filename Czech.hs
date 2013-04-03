module Czech ( stripDiacritics
             , czechDateField
             , czechPandocTransform
             ) where

import Hakyll (dateFieldWith, Context(..))
import Data.Char (isAscii, toLower)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import System.Locale
import Text.Pandoc (bottomUp, Pandoc(..), Inline(..))

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

conjuctions :: [String]
conjuctions = ["a", "i", "k", "o", "s", "u", "v", "z"]

nbsp :: String
nbsp = " "

addNonBreakingSpaces :: [Inline] -> [Inline]
addNonBreakingSpaces [] = []
addNonBreakingSpaces (Str s : Space : xs)
    | map toLower s `elem` conjuctions = Str (s++nbsp) : addNonBreakingSpaces xs
    | otherwise = Str s : Space : addNonBreakingSpaces xs
addNonBreakingSpaces (x:xs) = x : addNonBreakingSpaces xs

czechPandocTransform :: Pandoc -> Pandoc
czechPandocTransform = bottomUp addNonBreakingSpaces
