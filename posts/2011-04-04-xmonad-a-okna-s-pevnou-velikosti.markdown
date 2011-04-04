---
title: XMonad a okna s pevnou velikostí
tags: XMonad, Haskell, programování
description: krátké shrnutí boje s okny s pevnou velikostí
---

Už docela dlouho k téměř úplné spokojenosti používám XMonad jako správce oken.
Sice má své mouchy, ale přednosti to dostatečně vyvažují. Navíc se i ty mouchy
dají odstranit.

Poslední věc, na kterou jsem narazil a která mě donutila upravit `xmonad.hs`
bylo ne zrovna přátelské chování k oknům, které si nastavují nějakou
požadovanou velikost. XMonad tyto rady totiž totálně ignoruje. Ve většině
případů je to docela žádoucí chování, jediná výjimka jsou okna, u kterých nemá
moc smysl měnit velikost. V mém případě šlo hlavně o [Frozen
Bubble][frozen-bubble] a [dosbox][dosbox].

Jedna možnost řešení by bylo vyjmenovat dotyčné aplikace a přímo oknům nastavit
režim floating. To se mi ale nelíbilo, protože by to nebylo ani pohodlné, ani
spolehlivé, a často bych musel měnit konfiguraci.

Společným znakem oken, která mají fixní velikost, je nastavení minimální a
maximální velikosti okna na stejnou hodnotu. Toto nastavení je
v `WM_NORMAL_HINTS` a dá se zjistit třeba přes `xprop`. Toto pole má typ
`XSizeHints` a nejpřesnější popis, který jsem našel, vypadá takto:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.c}
typedef struct {
    long flags;                /* marks which fields
                                  in this structure
                                  are defined */
    int x, y;                  /* Obsolete */
    int width, height;         /* Obsolete */
    int min_width, min_height;
    int max_width, max_height;
    int width_inc, height_inc;
    struct {
           int x;              /* numerator */
           int y;              /* denominator */
    } min_aspect, max_aspect;
    int base_width, base_height;
    int win_gravity;
    /* this structure may be extended in the future */
} XSizeHints;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To jistě není žádný zázrak, ale už se s tím dá něco dělat. Zajímavé hodnoty
jsou ty s prefixem `min_` a `max_`. Pokud jsou nastavené na jinou hodnotu než 0
a zároveň se rovnají odpovídající se hodnoty, okno má asi nastavenou fixní
velikost.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
isFixed :: [CLong] -> Bool
isFixed h = minWidth h == maxWidth h
         && minHeight h == maxHeight h
         && all (>0) [minWidth h, maxWidth h, minHeight h, maxHeight h]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

V modulu `XMonad.Util.WindowProperties` je k dispozici funkce
`getProp32s :: String -> Window -> X (Maybe [CLong]`, s jejíž pomocí už se dá
napsat potřebná funkce pro manage hook. Definice typu `CLong` je v modulu
`Foreign.C.Types`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
isNonResizable :: Query Bool
isNonResizable = ask >>= \w -> liftX $ do
    atom <- getProp32s "WM_NORMAL_HINTS" w
    return $ case atom of
        Just hints  -> hasFixedSize hints
        Nothing     -> False
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Poslední chybějící kus jsou funkce na vrácení požadovaných rozměrů. Vlastně jde
jenom o čitelnější zápisy vytažení příslušné hodnoty ze seznamu:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
minWidth  = (!! 5) :: [CLong] -> CLong
minHeight = (!! 6) :: [CLong] -> CLong
maxWidth  = (!! 7) :: [CLong] -> CLong
maxHeight = (!! 8) :: [CLong] -> CLong
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A to je asi tak všechno. Zatím mám pocit, že to funguje, žádné chyby jsem
nepozoroval.

[frozen-bubble]: http://www.frozen-bubble.org/
[dosbox]: http://www.dosbox.com/
