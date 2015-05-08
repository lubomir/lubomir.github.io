---
title: Popisná statistika v R
tags: statistika, R
---

Tento text je výcuc z mých poznatků, které jsem nabyl při vypracování projektu
do jistého matematického předmětu.


## Načítání dat a první průzkum

V první řadě je potřeba načíst soubor s daty. Za předpokladu, že máme data ve
formátu CSV, se nám bude hodit příkaz `read.csv` (jak nečekané…). Taky by bylo
dobré zjistit, jak vlastně data vypadají, tedy jaké jsou názvy atributů a jak
vypadá prvních pár záznamů.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
> data <- read.csv("nazev_souboru.csv")
> names(data)
...
> head(data)
...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Jednotlivé atributy je potom možné adresovat přes notaci se znakem `$`, tedy
třeba `data$atr1`. Funkce `nrow(data)` nám prozradí, kolik je v datovém rámci
záznamů.

Pro kvalitativní atributy se hodí výpis hodnot s příslušnou četností. To se dá
udělat pomocí funkce `table`. Pokud bychom chtěli relativní četnosti, stačí
celou tabulku podělit počtem řádků.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
> table(data$class)

   1    2
 700  300
> table(data$class) / nrow(data)

  1   2
0.7 0.3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pro atributy s větším počtem hodnot než jenom dvě je možné vygenerovat tabulku
kumulativních relativních četností pomocí funkce `cumsum()`.

Pro vizualizaci kvalitativních atributů se hodí např. sloupkový diagram, který
je možné vytvořit pomocí funkce `barplot` aplikované na tabulku četností (třeba
i kumulativních). Jiná možnost je polygon četností – funkce `plot`, jejíž první
argument je tabulka, kterou chceme vizualizovat. Polygon četností navíc
vyžaduje argument `type="b"`, tedy typ grafu *both*, což znamená puntíky
spojené čarami.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
> barplot(table(data$class))
> plot(table(data$at7), type="b")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Další užitečné atributy pro grafy jsou:

`main`
  : hlavní titulek umístěný nad grafem
`xlab`, `ylab`
  : popisek osy x, resp. osy y
`col`
  : vektor barev použitý pro graf, případně jedna barva jako řetězec,  význam
    se zřejmě liší pro jednotlivé typy grafu; např. `col=c("red","green","blue")`
`pch`
  : typ puntíku
`cex`
  : velikost vykreslovaných puntíků


## Kontingenční tabulky

Závislosti mezi jednotlivými atributy je možné zkoumat třeba pomocí
kontingenční tabulky. Tu opět produkuje funkce `table`, tentokrát se dvěma
argumenty. První argument (a tedy datová sada) určuje řádky, druhý argument
jsou sloupce.

Z této tabulky je možné dopočítat kontingenční tabulku řádkově (nebo sloupcově)
podmíněných četností. Nenašel jsem k tomu ale vestavěnou funkci, takže to bude
vyžadovat trochu černé magie.

Pro řádkově podmíněné četnosti stačí každou hodnotu v kontingenční tabulce
absolutních četností podělit součtem hodnot na příslušném řádku. Součty po
řádcích je možné vyprodukovat pomocí funkce `rowSums`. Tabulky potom stačí
podělit.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
> table(atribut1, atribut2) / rowSums(table(atribut1, atribut2))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Se sloupcově podmíněnými četnosti je to trochu horší. Existuje funkce
`colSums`, která spočítá sumy přes sloupce, jednoduché dělení jako v předchozím
případě ale nebude fungovat, protože tabulky si nebudou rozměrově správně
odpovídat. Řešením je využít funkci s lakonickým názvem `t`, která transponuje
svůj jediný argument.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
> tab <- table(atribut1, atribut2)
> t(t(tab) / colSums(tab))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Správnost výsledků se dá ověřit například pomocí dalšího volání `rowSums`, V
tabulce řádkově podmíněných četností by měla být suma na každém řádku rovna 1,
obdobně pro sloupce pro sloupcově podmíněné četnosti.


## Generování histogramů

Základní histogram se dá udělat pomocí funkce `hist`. Bez nějakých dalších
argumentů ale vytvoří nehezkou obludu. Hlavní kámen úrazu této funkce je
nastavování počtu intervalů. Pomocí argumenty `breaks` je sice možné zadat
číslo nebo název metody, ale je to jenom doporučení a funkce se tím nemusí
přesně řídit. Navíc výchozí nastavení vygeneruje nepříliš přehlednou osu $x$,
ze které se těžko odečítají hranice intervalů.

Nicméně můžeme tady využít možnosti definice vlastních funkcí a napsat vlastní
vykreslování histogramů. Tato funkce bude očekávat data ve stejném formátu jako
standardní `hist`, počet intervalů, a volitelný popisek osy $x$.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
myhist <- function(data, num_bins, lab='data') {
    # Spočítáme šířku intervalu s přesností na jedno desetinné místo
    width <- round((max(data) - min(data)) / num_bins, digits=1)
    # Uděláme vektor bodů, kde začínají a končí intervaly
    breaks <- seq(min(data), min(data) + num_bins * width, by=width)
    # Pro jistotu vypíšeme šířku intervalu na výstup
    cat('Bin width = ', width, '\n')

    # Nastavíme okraje
    par(mar=c(6, 4, 1, 2))
    hist(data, breaks=breaks,
        xaxt='n',               # Nechceme osu X
        xlab='',                # Ani její popisek
        main='',                # I hlavní titulek vynecháme
        col='lightblue',        # Barva výplně sloupců
        border='blue',          # Barva rámečku sloupců
        ylab='Absolutní četnost')
    # Přidáme vlastní osu
    axis(1,                     # Kreslíme osu X
        at=breaks,              # Chceme značky na hranicích intervalů
        las=2)                  # Test bude vertikálně, aby se to tam vešlo
    # Vykreslíme popisek pod osou
    mtext(lab, side=1, line=4)  # Číslo u line udává, jak daleko bude text od
                                # grafu

    # Volitelně bychom mohli ještě do rohu přidat informaci o šířce intervalu
    leg <- paste('Šířka intervalu', format(width))
    legend('topright', leg, box.lty=0)
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Jako argumenty `col` a `border` funkce `hist` by bylo možné zadat i vektory
barev, kdybychom to chtěli oživit. Nechceme.

Velikost fontu se dá ovlivnit parametrem `cex`. Je potřeba ho zadat u `par` a
`mtext`.

![Ukázka vytvořeného histogramu](/images/r-histogram.png)


## Korelace a regresní přímka

Když už máme dva numerické atributy, můžeme mezi nimi hledat hledat nějaké
vztahy. Korelační koeficienty se dají počítat funkcí `cor(atribut1, atribut2,
method="pearson")`. Další dostupné metody jsou `spearman` a `kendall`.

Vypočítat koeficienty regresní přímky lze pomocí funkce `lm`. Vykreslení do
grafu potom provede funkce `abline` aplikovaná na to, co vrátí `lm`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
> lm(loans ~ ages)
...
Coefficients:
(Intercept)         ages
   2982.684        8.118
> abline(lm(loans ~ ages))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Před samotným vykreslením přímky je možné si nachystat třeba tečkový diagram
atributů, jejichž závislost hledáme.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.R}
> plot(ages,        # Co bude na ose X
       loans)       # Co bude na ose Y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Ukládání do souborů

Uložení vytvořeného grafu do souboru je vlastně naprosto triviální. Před
voláním vykreslovacích funkcí je potřeba přesměrovat výstup do požadovaného
souboru pomocí funkce, jejíž název určí formát výstupu a jediný argument název
výstupního souboru. Dostupné jsou minimálně `pdf` a `png`. Až je dokresleno,
zavolání `dev.off()` zase přesměruje výstup na obrazovku.


## Další užitečné funkce

* Funkce `source` jako argument očekává jméno souboru a chová se jako
  stejnojmenný příkaz v Bashi.
* Zaokrouhlení hodnot na určitý počet desetinných míst se dá provést pomocí
  `round(x, digits=N)`, kde `x` může být číslo nebo klidně i tabulka.
* Pokud před název funkce napíšeme otazník, dostaneme poměrně podrobnou
  nápovědu. Např. `?table`.

## Odkazy

* [Pěkný úvod do popisné statistiky v R](http://www.r-tutor.com/elementary-statistics)
