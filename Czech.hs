module Czech where

import Hakyll (renderDateFieldWith)
import System.Locale

-- |Replace accents from above letters.
stripDiacritics :: String -> String
stripDiacritics str = foldl (\s (f,t) -> replace f t s) str diacritics

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace f r (x:xs)
  | f == x    = r : replace f r xs
  | otherwise = x : replace f r xs

diacritics :: [(Char, Char)]
diacritics =  zip "ěščřžýáíéĚŠČŘŽÝÁÍÉúů"
                  "escrzyaieESCRZYAIEuu"

renderCzechDate field = renderDateFieldWith cs field "%-d. %B %Y" "Neznámé datum"

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
