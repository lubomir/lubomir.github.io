---
title: Znásilňování SICStus Prologu
description: přiohnutí interpretu Prologu
tags: prolog, Debian, vim
---


## Použití

Při instalaci se do adresáře `$PREFIX/bin` mimo jiné nainstaluje spustitelný
soubor `sicstus`, což je přímo interpret. Tu stačí spustit a jsem skoro tam,
kde jsme chtěli být.

To slovíčko „skoro“ je poměrně důležité. Interpret sice běží, ale
zdaleka se nechová tak, aby se dal pohodlně používat. Například kurzor se nedá
posunout šipkami doprostřed psaného dotazu a opravit překlep, po ukončení
zůstane nezalomený řádek a posunutý prompt, nikde žádná historie dotazů …

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
možnosti. Osobně bych preferoval obarvení pouze klasické výzvy `| ?- ` a
podobných, které začínají znakem `|`.

Naštěstí si tohle přání můžu splnit pomocí filtru. Stačí si uložit [tento
filtr][filtr] ideálně do adresáře `/usr/share/rlwrap/filters` a potom spouštět
pomocí `rlwrap -z 'prolog_filter Blue' sicstus`. Možnosti barev jsou `black`,
`red`, `green`, `yellow`, `blue`, `purple`, `cyan` a `white`. Pokud je alespoň
první písmeno velké, použije se tučná varianta.

#### Aktualizace 16. 3. 2011

Existuje i výrazně jednodušší řešení. Rlwrap umožňuje přepínači nastavit, které
výzvy má „vařit“. Je to přepínač `-O regexp`, kde potřebný
regulární výraz je `'^\|'`. Potom stačí použít klasicky `-pBARVA` (stejný výčet
jako je výše) a už to funguje.

Taky se může hodit přidat přepínač `-g '^.$'`, což zakáže ukládání
jednopísmenných odpovědí do historie.

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


[rlwrap]:   https://github.com/hanslub42/rlwrap
[vimfile]:  https://github.com/adimit/prolog.vim
[filtr]:    /data/prolog_filter
