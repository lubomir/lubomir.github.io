---
title: Zpracování PDF
tags: pdf, unix, bash, různé
---

Občas se mi dostane do rukou naskenovaná knížka ve formátu PDF, kde jsou vždy
dvě stránky původního textu na jedné virtuální stránce. Člověk se potom musí
posouvat nejen směrem dolů, ale i do stran a občas nahoru. Hlavně na mobilu nic
moc.

Nakonec jsem našel relativně spolehlivý způsob, jak takový monolit rozporcovat
na menší kousky, a abych to nezapomněl, tak si to poznamenám.

Budeme potřebovat [pdftk], [imagemagick] a [poppler-utils]. V Debianu je
všechno nachystané v repozitářích.

[pdftk]: http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/
[imagemagick]: http://www.imagemagick.org/script/index.php
[poppler-utils]: http://poppler.freedesktop.org/

## Nasekat na jemno, prosím

Nejdřív původní soubor nasekáme na jednotlivé stránky. Následující příkaz
vytvoří z každé stránky v původním pdf jeden soubor.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.bash}
$ pdftk VSTUPNI.PDF burst
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tímto vznikne plus minus autobus milion souborů s názvy `pg_0001.pdf`.

## Protřepat, nemíchat

Následující krok by měl každý vytvořený soubor vertikálně rozpůlit. K tomu se
dá použít třeba programů z balíku `poppler-utils`. Abychom se s tím zbytečně
nepárali, vezmeme to všechno naráz.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.bash}
for i in pg_*pdf; do
    pdftoppm $i >${i/pdf/ppm}
    convert $(i/pdf/ppm} -crop 50%x100% +repage vystup-$i
done
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Jakkoli děsivě ta substituce vypadá, dělá jen jednu věc: nahradí příponu `pdf`
příponou `ppm`. Následující příkaz z balíku `imagemagick` potom akorát obrázek
rozsekne napůl (tady se dá pohrát s konkrétním číslem) a výsledek uloží jako
dvoustránkové PDF do souboru `vystup-pg_0001.pdf`.

## Matláma patláma paprčála

Teď už stačí znovu použít `pdftk` a všechno to zase nakombinovat do jednoho
souboru.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.bash}
$ pdftk vystup-* cat output spojeny.pdf
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Zjevně není nutné vytvářet pouze jeden monolit, ale jde to rozdělit do souborů
třeba po kapitolách. Taky je snadné vyhodit nezajímavé stránky.

## Optimalizace velikosti

Popis výš má zásadní problém: vytvořený soubor má rozměry slona po obědě.
Možnou odtučňovací kůrou je třeba `ghostscript`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.bash}
$ gs -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dNOPAUSE \
> -dBATCH -sDEVICE=pdfwrite -sOutputFile=output.pdf spojeny.pdf
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tím se každá stránka přepočítá na plus minus 72 dpi, což je dost pro monitor.
Na tisk už to ale stačit nebude. Další možnosti jsou `/ebook`, `/printer`,
`/prepress`.
