---
title: Inicializační funkce NCurses
abstract: velice krátký překlad části manuálu
---

Tato stránka je částečným překladem
[tutoriálu NCurses](http://tldp.org/HOWTO/NCURSES-Programming-HOWTO/),
konkrétně [této části](http://tldp.org/HOWTO/NCURSES-Programming-HOWTO/init.html).

## 4.1 Inicializační funkce

1. **raw() a cbreak()**

    Normálně ovladač terminálu ukládá uživatelem zadané znaky do bufferu,
    dokud uživatel nestiskne enter. Většina programů ale potřebuje, aby byly
    znaky dostupné hned, když je uživatel zadá. Tyto dvě funkce zakazují
    bufferování řádků. Rozdíl mezi nimi je ve zpracování ovládacích znaků
    zadávaných z klávesnice (např. Ctrl-Z, Ctrl-C). Po zavolání raw() jsou
    tyto znaky předány programu a nevygenerují signál. V režimu cbreak() jsou
    tyto znaky interpretovány přímo ovladačem terminálu.
  
2. **echo() a noecho()**

    Tyto funkce ovládají, jestli se znaky zadávané uživatelem zobrazují
    na obrazovku či nikoli. Většina interaktivních programů volá na začátku
    noecho() a pokud to potřebuje, tak znaky vypisuje kontrolovaně sama.

3.  **keypad()**

    Tato inicializační funkce umožňuje čtení funkčních kláves jako F1,,
    kurzorové klávesy atd. Téměř každý interaktivní program toto povoluje,
    protože šipky jsou zásadní částí každé uživatelského rozhraní. Abyste
    povolili tuto vlastnost pro standardní obrazovku (stdscr), zavolejte
    `keypad(stdscr,TRUE)`.

4. **halfdelay()**

    Přestože tato funkce není používaná příliš často, je občas velice
    užitečná. Vstup je okamžitě předán programu jako po zavolání
    `cbreak()` s tím rozdílem, že čeká X desetin sekundy na vstup
    a jestliže potom není vstup dostupný, vrátí ERR. X je interval zadaný
    jako argument při volání halfdelay(). Tato funkce je užitečná, pokud
    potřebujete nějaký vstup, ale když uživatel neodpoví dostatečně rychle,
    chcete udělat něco jiného.
