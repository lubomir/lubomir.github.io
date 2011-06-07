---
title: Kódování čísel
tags: kódování, Haskell, programování
description: Ukázka implementace různých variant kódování přirozených čísel
---

V předmětu [IB047][ib047] se dá mimo jiné potkat s problémem, jak co
nejúsporněji uložit data do souboru. Jako ideální řešení se ukáže použít nějaké
vhodné kódování. A protože nejlépe člověk něco pochopí tak, že si to vyzkouší,
napsal jsem si funkce pro kódování i dekódování různými metodami.

## Varianty

První myslitelné kódování je klasický zápis čísla ve dvojkové soustavě. Jeho
výhoda je velká úspornost, na druhou stranu každá posloupnost nul a jedniček se
dá interpretovat jako číslo. My bychom ale chtěli poznat, kde jedno číslo
končí a druhé začíná. Proto potřebujeme *bezprefixový* kód, tedy takový, aby
žádné slovo nebylo prefixem jiného. Zároveň ale nechceme používat žádnou
překladovou tabulku, takže [Huffmanovo kódování][huffman] je ze hry.

Nejjednodušší variantou, jak toho dosáhnout, je použít *unární* kódování. Číslo
$n$ zakóduje pomocí $n$ nul následovaných jedničkou. Zřejmě je tedy
bezprefixové, ale zároveň i paměťově neefektivní, protože délka kódu je
lineární k velikosti kódovaného čísla.

Jako lepší nápad vypadá zapsání čísla binárně a přidání jeho délky pomocí
nějakého bezprefixového kódu. To ale také není optimální, protože bychom
nevyužili mnoho možností. Konkrétně jde o slova, která začínají nulami.
V klasickém dvojkovém zápisu jsou čísla 00 a 0 stejná. Z našeho pohledu je ale
užitečné je rozlišit. Proto pro každou délku kódu využijeme všechny možnosti,
tedy pro slovem délky jedna budeme kódovat dvě čísla, délkou dvě čtyři atd.

Pro každé číslo tedy určíme délku slova, odečteme od něj nějakou hodnotu a
převedeme do dvojkové soustavy. Délku určíme jako
$\lfloor\log_2_{}(n+1)\rfloor$. Zároveň z délky můžeme určit, o kolik zmenšit
kódované číslo před převodem: $2^l-1$.


## Implementace

Nejdříve tedy definice typů.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
data Bit = Zero | One deriving (Eq)

instance Show Bit where
    show Zero = "0"
    show One  = "1"

type Stream = [Bit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Typy požadovaných funkcí vypadají následovně. Kódovací funkce pro číslo vrátí
posloupnost bitů, dekódovací naopak z proudy bitů vytáhnou první číslo a vrátí
ho společně se zbytkem proudu. Funkce `fromBinary` se liší v tom, že zpracuje
celý proud.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
unary, binary, gamma, delta        :: Int -> Stream
parseUnary, parseGamma, parseDelta :: Stream -> (Int, Stream)
fromBinary :: Stream -> Int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unární kódování je velice jednoduché. Operátor `(***) :: (a -> b) -> (c -> d)
-> (a, c) -> (b, d)` se nachází v modulu `Control.Arrow` a jeho jediný účel je
aplikace funkcí na složky dvojice.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
unary n    = replicate n Zero ++ [One]
parseUnary = (length *** tail) . span (== Zero)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Funkce pro binární kódování čísla jsou trošku složitější, ale pořád tam není
žádná magie. Provádí klasický přepis čísla do dvojkové soustavy. Pomocná funkce
`binary'` navíc výsledný kód doplní zleva do požadované délky nulami. Také 0
při délce kódu 0 reprezentuje prázdným seznamem.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
binary = bin []
  where
    bin acc 0 = Zero : acc
    bin acc 1 = One  : acc
    bin acc n = bin ((if odd n then One else Zero) : acc) (n `div` 2)

binary' :: Int -> Int -> Stream
binary' 0 0 = []
binary' len n = padding ++ code
  where code = binary n
        padding = replicate (len - length code) Zero

fromBinary = foldl (\num x -> num * 2 + (if x == Zero then 0 else 1)) 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Myšlenku popsanou na začátku realizují mimo jiné $\gamma$ a $\delta$ kódy.
$\gamma$-kód pro délku slova používá unární kódování, $\delta$-kód jde ještě
dál a používá pro zapsání délky $\gamma$-kód. Zřejmě jsou si tedy v mnohém
podobná a má smysl nejdřív napsat obecnější funkci.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
encodeWith :: (Int -> Stream) -> Int -> Stream
encodeWith f n = f len ++ binary' len (n - 2^len + 1)
  where len = floor . logBase 2 . fromIntegral $ n + 1

parseWith :: (Stream -> (Int, Stream)) -> Stream -> (Int, Stream)
parseWith f s = (fromBinary bin + 2^len - 1, ss)
  where (len, rest) = f s
        (bin, ss)   = splitAt len rest
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

S funkcemi `encodeWith`, `parseWith` je tedy implementace $\gamma$ i $\delta$
kódování poměrně triviální.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
gamma = encodeWith unary
parseGamma = parseWith parseUnary

delta = encodeWith gamma
parseDelta = parseWith parseGamma
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Odtud už je jednoduché napsat funkci, které bude kódovat nebo dekódovat seznam
čísel.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
encode :: (Int -> Stream) -> [Int] -> Stream
encode = concatMap

decode :: (Stream -> (Int, Stream)) -> Stream -> [Int]
decode f s = let (n, ss) = f s
             in case ss of
                  [] -> [n]
                  _  -> n : decode f ss
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Upozornění**: toto není úplně ideální implementace, protože umožňuje práci
pouze s čísly omezené velikosti, navíc jsem se nesnažil o efektivitu, takže je
to velice pomalé. Taky chybí jakákoli kontrola chyb.


[ib047]: https://is.muni.cz/auth/predmety/predmet.pl?id=585364
[huffman]: http://cs.wikipedia.org/wiki/Huffmanovo_kódování
