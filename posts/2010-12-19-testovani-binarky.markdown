---
title: Testování binárky
abstract: jednoduchá metoda kontroly spustitelného souboru
tags: programování, bash, testy
---

Pro usnadnění testování úkolů do PB071 a PB161 jsem si napsal jednoduchý
skript. Vlastně jenom vezme spustitelný soubor, postupně mu předhazuje
definované vstupy a kontroluje, co je na standardním výstupu, chybovém
výstupu a s jakým kódem skončil. Volitelně se to celé dá prohánět
valgrindem.

## Formát testů

Všechny testy se ukládají do speciálního adresáře. Já používám název `tests`,
ale prakticky na tom nezáleží. Každý tests se skládá alespoň ze dvou souborů s
názvy třeba `01_valid_input.*`. Přípona souboru specifikuje, co soubor testuje.

 `*.in`
  : toto přijde na standardní vstup
 `*.args`
  : toto se předá jako argumenty (a zatím pořádně nefunguje, pokud některý
    argument má obsahovat mezeru.)
 `*.out`
  : očekávaný výstup na stdout
 `*.err`
  : očekávaný chybový výstup
 `*.ret`
  : návratový kód programu

Pokud některý soubor neexistuje, tak se příslušná část netestuje.

## Použití

Stáhněte si [archiv s testovacím skriptem]($root/data/stest.tar.bz2) a pomocným
souborem, který umožňuje vypisovat výsledky barevně. Skript `stest` je dobré
mít v cestě, `.term_colors` může být buď přímo v domovském adresáři nebo na
stejném místě jako `stest`.

K testování stačí spustit příkaz `stest ./NAZEV_BINARKY`, případně přidat ještě
cestu k adresáři s testy. Další možnosti shrnuje nápověda `stest --help`.

