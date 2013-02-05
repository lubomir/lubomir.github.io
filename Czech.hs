module Czech where

import Hakyll (dateFieldWith)
import Data.Char (isAscii)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import System.Locale

-- |Remove accents from above letters.
stripDiacritics :: String -> String
stripDiacritics = T.unpack . T.filter isAscii . ICU.normalize ICU.NFD . T.pack

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
