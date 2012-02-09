---
title: Rychlost prohledávání adresáře
description: porovnání různých metod pro vypsání souborů a podaresářů
tags: C, programování, benchmark
---

Procházení všech souborů v adresáři je velice často se objevující potřeba.
Navíc je v takovém případě obvykle potřeba rozlišit, jestli nalezená položka je
soubor, adresář nebo ještě něco jiného. Je řada možností, jak tohle provést.
Zkusil jsem porovnat, jak moc se časově liší různé varianty programu, který
projde všechny položky adresáře a nejdříve vypíše soubory, potom adresáře.

## Hřiště

Použil jsem tři různé testovací adresáře. Jeden obsahoval jenom soubory, a to
třicet tisíc, druhý obsahoval třicet tisíc adresářů a třetí po patnácti
tisících od obojího. Počty v třetím případe nejsou naprosto přesné, adresářů
tam může být o něco méně. Způsobené je to tím, že názvy jsou generovány skoro
náhodně, aby se zajistilo náhodné pořadí.

Vygenerovat to jde dvěma jednoduchými cykly v Bashi.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.bash}
for i in $(seq 1 30000); do
    touch bench1/file-$i
    mkdir bench2/dir-$i
done

for i in $(seq 1 15000); do
    touch bench3/$(mkpasswd "file-$i" | sed "s@/@_@g")
    mkdir bench3/$(mkpasswd "dir-$i" | sed "s@/@_@g")
done
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Metody

První rozdíl v testovaných programech spočíval v tom, jak se zajistí, aby
soubory byly vypsané dříve než adresáře. Nejjednodušší přístup spočívá v tom,
že se adresář projde dvakrát, v prvním průchodu se ignorují adresáře, v druhém
soubory.

Druhá a třetí metoda adresář procházejí jenom jednou a nalezené adresáře si pro
pozdější zpracování ukládají buď do pole nebo spojového seznamu. Pole je po
naplnění vždycky potřeba zvětšit, pokud se ale jeho velikost vždycky
zdvojnásobí, složitost uložení nakonec vyjde konstantní. Každý uložený řetězec
je taky potřeba naalokovat. Spojový seznam má sice taky konstantní složitost
přidání prvku, ovšem vyžaduje na rozdíl od pole dvě alokace paměti.

Druhý rozdíl v testovaných programech spočívá v metodě určování typu položky.
Použil jsem zase tři možnosti: pomocí funkce `stat()`, pomocí položky `d_type`
ve struktuře `dirent` a pomocí lehce uhozené heuristiky s `opendir()`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.c}
int is_dir(const char *path)
{
    struct DIR *dir = opendir(path);
    if (dir == NULL) {
        return 0;
    } else {
        closedir(dir);
        return 1;
    }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tato heuristika má ovšem zásadní problém v tom, že i adresáře, které se
nepodařilo otevřít třeba kvůli oprávněním, nahlásí jako soubory.

Rozpoznávání pomocí `d_type` je nejjednodušší, ale opět má nevýhody:
 + nemusí existovat (pozná se podle makra `_DIRENT_HAVE_D_TYPE`
 + i když existuje, může (v závislosti na souborovém systému) obsahovat hodnotu
   `DT_UNKNOWN` a tedy je potřeba se s tím vyrovnat

## Výsledky

Každý z devíti programů jsem spustil stokrát a výsledné časy zprůměroval.
Metodika nic moc, ale nějaká představa se z toho udělat dá.

<table>
<tr><th></th>           <th>Soubory</th> <th>Adresáře</th> <th>Mix</th></tr>
<tr><th>hvojitý průchoh</th><th></th><th></th><th></th></tr>
<tr><td>`stat()`</td>   <td>0,109</td>   <td>0,106</td>    <td>0.107</td></tr>
<tr><td>`opendir()`</td><td>0,133</td>   <td>0,177</td>    <td>0.155</td></tr>
<tr><td>`d_type`</td>   <td>0,043</td>   <td>0,040</td>    <td>0.042</td></tr>
<tr><th>Pole</th><th></th><th></th><th></th></tr>
<tr><td>`stat()`</td>   <td>0,054</td>   <td>0,057</td>    <td>0.053</td></tr>
<tr><td>`opendir()`</td><td>0,067</td>   <td>0,091</td>    <td>0.080</td></tr>
<tr><td>`d_type`</td>   <td>0,021</td>   <td>0,021</td>    <td>0.020</td></tr>
<tr><th>Seznam</th><th></th><th></th><th></th></tr>
<tr><td>`stat()`</td>   <td>0,053</td>   <td>0,060</td>    <td>0.058</td></tr>
<tr><td>`opendir()`</td><td>0,065</td>   <td>0,091</td>    <td>0.081</td></tr>
<tr><td>`d_type`</td>   <td>0,020</td>   <td>0,029</td>    <td>0.022</td></tr>
</table>

Ponaučení na závěr? Na metodě procházení příliš nezáleží. To, co se s
nalezenými položkami bude dít, pravděpodobně zabere výrazně víc času, takže
jinou metodou procházení se nedá nic ušetřit.

Všechny [zdrojové soubory](/data/benchmark-dir.tar.bz2) je možné si stáhnout
včetně měřících skriptů. Skript `prepare.sh` vytvoří testovací složky,
`measure.sh` spustí měření a `test.sh` ověřuje, že všechny programy dávají
stejný výstup na třetím testovacím adresáři.
