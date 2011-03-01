---
title: Znásilňování SICStus Prologu
tags: prolog, Debian, vim
---


## Instalace

Nejdřív je potřeba stáhnout samotný program k instalaci z [této
stránky][download] (pouze pro studenty FI a nekomerční použití). Přihlašuje se
učem a sekundárním heslem. Je nutné stáhnout dva soubory: archiv s programem a
soubor s příslušným licenčním klíčem.

Instalace je docela jednoduchá:

1. rozbalit archiv
2. v nově vytvořeném adresáři spustit `./InstallSICStus --all-questions`
3. odpovědět na spoustu otázek
    * klíč a podobné věci jsou v druhém staženém souboru
    * pokud se zvolí vhodný adresář, není potřeba ani heslo roota
    * instalovat Tk, jasper, odbc ani nic podobného není nezbytně nutné
    * pokud se instaluje v systému s vyšší verzí GLibc než 2.7, je potřeba
      potvrdit, že opravdu chci instalovat

A to je asi tak vše. Další pokračování by mohla být instalace
[Eclipse][eclipse] a IDE pro Prolog jménem [Spider][spider].

Případná odinstalace je relativně jednoduchá. Stačí si ponechat soubor
`UnInstallSICStus` z adresáře, odkud se instalovalo. Je to primitivní shellový
skript, který smaže všechny nainstalované soubory. Komentář v souboru sice
tvrdí, že některé soubory neodstraní, ale nikde jsem žádné zbytky nenašel.


## Použití

Při instalaci se do adresáře `$PREFIX/bin` mimo jiné nainstaluje spustitelný
soubor `sicstus`, což je přímo interpret. Tu stačí spustit a jsem skoro tam,
kde jsme chtěli být.

To slovíčko &bdquo;skoro&ldquo; je poměrně důležité. Interpret sice běží, ale
zdaleka se nechová tak, aby se dal pohodlně používat. Například kurzor se nedá
posunout šipkami doprostřed psaného dotazu a opravit překlep, po ukončení
zůstane nezalomený řádek a posunutý prompt, nikde žádná historie dotazů ...

Naštěstí to není nezvratný stav. Stačí nainstalovat utilitu [rlwrap][rlwrap]
třeba ze standardních repozitářů a potom spouštět `rlwrap sicsus`. Výsledkem je
stejný interpret, ale tentokrát se všemi výhodami knihovny readline, jako třeba
funkční šipky nebo historie ukládaná do `~/.sicstus_history`, ve které se dá
interaktivně vyhledávat přes `^R` nebo jenom šipkou nahoru/dolů. Možná to má
někde nějaký háček, ale zatím jsem na nic nepřišel.


### Někdo to rád barevné

Rlwrap umožňuje pomocí přepínače `-p` (nebo `--promp-colour`) obarvit výzvu
programu. Menší problém je ale v tom, že za prompt považuje i řádky, kde se
vypisuje víc možných unifikací a interpret čeká na středník kvůli další
možnosti. Osobně bych preferoval obarvení pouze klasické výzvy `| ?- ` (a
případně i jiných, pokud existují).

Naštěstí si tohle přání můžu splnit pomocí filtru. Stačí si uložit [tento
filtr][filtr] ideálně do adresáře `/usr/share/rlwrap/filters` a potom spouštět
pomocí `rlwrap -z 'prolog_filter Blue' sicstus`. Možnosti barev jsou `black`,
`red`, `green`, `yellow`, `blue`, `purple`, `cyan` a `white`. Pokud je alespoň
první písmeno velké, použije se tučná varianta.

## Vim

Vim sám o sobě Prolog moc nepodporuje. Nakopírováním těch [správných
souborů][vimfile] do `~/.vim/` se ale změní do celkem použitelné formy:
sympaticky zvýrazňuje syntaxi, automaticky odsazuje řádky a sám pozná, kde
začínají a končí foldy.

Poslední drobný zádrhel je v tom, že si většinou splete typ souboru s Perlem,
který taky používá příponu `.pl`. Dá se to obejít přidáním modeline na konec
souboru:

~~~~~~~~~~~~~~~~~~ {.prolog}
% vim: ft=prolog
~~~~~~~~~~~~~~~~~~~~~~~~~~~~


[download]: https://lindir.ics.muni.cz/sicstus/
[eclipse]:  http://www.eclipse.org/
[spider]:   http://www.sics.se/sicstus/spider/site/index.html
[rlwrap]:   http://utopia.knoware.nl/~hlub/rlwrap/
[vimfile]:  https://github.com/adimit/prolog.vim
[filtr]:    $root/data/prolog_filter
