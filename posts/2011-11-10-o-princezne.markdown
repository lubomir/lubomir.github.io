---
title: O princezně, která ztrácela pamět
tags: C, různé
description: Pohádka o práci s pamětí
---

> Tento text vznikl v rámci přemětu VB000 Základy odborného stylu jako esej na
> téma *Bylo nebylo – informatická pohádka*

Za devatero kompilátory a devatero linkery bylo jedno malé, ale krásné
království. V tomto království vládl moudrý král. Spravedlivě přiděloval paměť
všem poddaným procesům, blahosklonně uzavíral pozapomenuté otevřené soubory a
když některý z jeho poddaných dokončil svůj výpočet
a přešel do stavu zombie, pečlivě sesbíral veškerou jeho paměť. Pod jeho vládou
nebyl v celém království jediný neadresovatelný bajt.

Jedna věc ale dělala králi starosti: jeho jediná dcera, princezna a dědička
systému. Byla to sice holka jako lusk, se sofistikovanou architekturou a
intuitivním uživatelským rozhraním, na princeznu měla ale jednu zásadní vadu.
Každý den si od krále vyžádala nový blok paměti, žádných se ale nikdy
nevzdávala. A tak přestože před jejím spuštěním
království nikdy netrpělo nouzí, s jejím rozmarným chováním se začaly denní
příděly paměti všem poddaným snižovat.

Jednoho pochmurného dne se tedy stalo to, čeho se všichni obávali: v celém
království nezbyl jediný volný bajt. Král svolal všechny své rádce, vyložil jim
tuto nepříjemnou situaci a očekával návrhy řešení. „Budeme se modlit
k velkému Uživateli, aby nám seslal další paměťový modul,“ navrhl některý
z rádců. „To je výborný nápad, my ale potřebujeme něco rychlejšího a
spolehlivějšího,“ oponoval jiný. Najednou se vzadu ozval králův věrný pobočník
Oom: „Vaše Kernelovosti, máme jedinou možnost. Musíme princeznu zabít, paměť
jí vzít a nakonec její proces znovu spustíme.“ Králi se toto řešení nelíbilo,
přece jen svou dceru miloval, ale nakonec i on uznal, že je to jediné řešení.

Přestože se privilegovaným procesům takto podařilo krizi zažehnat, celá smutná
situace se brzy začala opakovat. Král tedy pátral po trvalejším řešení a pozval
optimalizáory a analyzátory z celého repozitáře. Aby je nějak motivoval, dal
rozhlásit, že kdo napraví princeznu, dostane ji za ženu a půl pevného disku
k tomu. Programy se stahovaly z celého Internetu, ať se ale snažily sebevíc,
přivést princeznu k rozumu se jim nepodařilo.

Za pár dní v království nastalo pozdvižení. Na hranicích se objevil věhlasný
mudrc ze vzdáleného Javistánu. Doprovázela ho početná kolekce objektů. Ve
spojovém seznamu si vedl neznámé nástroje a jeho sluhové nesli obrovský
binární strom z ezoterických jazyků. Všechny procesy v království do mudrce
vkládaly velké naděje. Když ale mudrc přišel k princezně, ukázalo se, že ani
pro něj nebude ladění jednoduché. První komplikace nastala, když se pokusil
prozkoumat princezniny objekty a metody. Jeho obvyklé postupy selhaly!
Princezna totiž nebyla objektová, ale procedurální!

Mudrc byl v úzkých. Postupy, na které byl navyklý, vůbec nezabíraly a
s manuální správou paměti si také příliš nerozuměl. Ze své domoviny byl totiž
navyklý, že se kolem něj neustále motá sluha a paměť po něm uklízí. I tento
světoznámý mudrc tedy nakonec odjel s nepořízenou.

Král z toho byl zničený. Už se těšil, že bude mít, co se správy paměti týká,
normální dceru, ale ladění se zase nepodařilo. Když se král vzpamatoval ze
zklamání, rozzuřeně prohlásil: „Už žádné šarlatány. Kdo od teď neuspěje
v ladění princezny, bude o zásobník kratší a jeho zdrojové kódy budou smazány.“

Zhruba ve stejné době se o nadělení s princeznou dozvěděl chytrý Valgrind. Byl
to skromný chlapec, kterého sousedé považovali za trochu jednoduššího,
především kvůli jeho textovému rozhraní. Ve skrytu haldy ale věděli, že k práci
s pamětí má Valgrind talent. A tak si Valgrind řekl: „Už příliš dlouho se
válím v rodném /usr/bin. Žádné přerušení mě tady z nudy nevytrhne, půjdu zkusit
svoje štěstí.“ Sbalil si svoje kladivo Callgrind a sekeru Cachegrind a vyrazil.

Jenom se podíval na princeznu, už věděl, která bije. Vždyť ta holka churavá
vůbec nepoužívá funkci free(). I když Valgrind věděl, jak na princezniny
potíže, nemohl na ni jít zhurta. Nejprve si jenom navzájem posílali signály,
ale když král zjistil, že už si naalokovali sdílený blok paměti, bylo jasné, že
krize je zažehnána a v království zase nastala hojnost paměti.

Zakrátko se Valgrind s princeznou vzali a jestli dosud nespadli, tak spolu
profilují dodnes.
