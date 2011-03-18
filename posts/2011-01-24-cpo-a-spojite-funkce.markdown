---
title: CPO a spojité funkce
tags: teorie, typy, vyčíslitelnost
---

Každý typ se dá charakterizovat jako částečně uspořádaná množina hodnot. Pokud
platí, že každý spočetný řetězec má supremum, potom se jedná o [Complete Partial
Order](http://en.wikipedia.org/wiki/Complete_partial_order).

## Primitivní typy -- příklady

![Unit_⊥_]($root/images/unitt.png)

![Bool_⊥_]($root/images/boolt.png)

Značky `tt` a `ff` jsou jednoduše symboly pro pravdivou a nepravdivou hodnotu.
Další primitivní typ je třeba typ všech celých čísel `Int`.

## Operace nad typy

Z primitivních typů je možné pomocí operací vytvářet další, bohatší typy.

 Lift X
  : přidá ⊥ jako nejméně definovaný prvek do typu X. Dno (⊥) je menší než
    všechny původní prvky v X a jinak se touto operací uspořádání nemění.
 X × Y
  : kartézský součin dvou typů (typ všech dvojic, kde první složka je typu X a
    druhá složka má typ Y). Pokud $x_1, x_2 \in X, x_1 \leq x_2$ a
    $y_1, y_2 \in Y, y_1 \leq y_2$, potom platí $(x_1,y_1) \leq (x_2,y_2)$.
    ^[drobný detail: pokaždé se jedná o jiné $\leq$, protože se porovnává
    v jiné doméně]
 X + Y
  : disjunktní sjednocení, hodnota tohoto typu obsahuje buď nějakou hodnotu
    typu X, nebo hodnotu typu Y (např. takto funguje `union` v C nebo `Either`
    v Haskellu)
 X → Y
  : mocninný typ; typ všech spojitých funkcí, které hodnotu typu X zobrazí na
    hodnotu typu Y

Další příklady jsou ve slidech z přednášky na straně 167.

![Bool_⊥_ × Bool_⊥_]($root/images/bool-x-bool.png)

## Spojitost funkce

> Funkce $f$ je monotonní právě tehdy, když platí
> $\forall a, b: a \leqslant b \Rightarrow f(a) \leqslant f(b)$.

Tedy: funkce $f$ je monotonní, pokud pro každé dva prvky jejího definičního
oboru platí, že pokud je jeden méně definovaný než jiný, tak potom jejich
obrazy na tom budou stejně.

Takže například funkce $f$ je monotonní, ale funkce $g$ spojitá není.

    f () = True
    f ⊥ = True

    g () = True
    g ⊥ = False

Spojitost funkce je ještě trochu silnější pojem. Každá spojitá funkce je i
monotonní. Obrácené tvrzení neplatí, existují i nespojité monotonní funkce.
Platí ale, že každá monotonní funkce s konečným definičním oborem (doménou) je
spojitá^[takže pro naprostou většinu příkladů na zkoušce PB006 stačí ověřovat
monotonicitu].

> Funkce $f$ je spojitá, pokud pro každý spočetný řetězec $x_1, x_2, \ldots$ platí
> $f(\bigsqcup(x_1,x_2,\ldots)) = \bigsqcup(f(x_1), f(x_2), \ldots)$

(Značka $\bigsqcup$ značí supremum, nejmenší horní závoru, least upper bound.
Horní závora nějaké podmnožiny je prvek, který je větší než libovolný prvek
dané podmnožiny.)

Definice tedy říká, že obraz suprema libovolného řetězce musí být supremem
obrazů jednotlivých prvků řetězce.

Česky: pokud se vezme libovolný spočetný řetězec a funkcí $f$ zobrazíme jeho
supremum, tak musíme dostat totéž, jako kdybychom postupně zobrazili všechny
prvky daného řetězce a našli supremum obrazů.

## Všechny Unit_⊥_ → Bool_⊥_

Zadání: Napište všechny spojité funkce typu Unit_⊥_ → Bool_⊥_. Co je to funkce
s tímto typem? Takováto funkce musí každé hodnotě z Unit_⊥_ přiřadit něco
z Bool_⊥_.

![Všechny funkce Unit_⊥_ → Bool_⊥_]($root/images/unit-bool2.png)

Tyto funkce by bylo vhodné taky seřadit do diagramu, aby bylo patrné, jak to
uspořádání na nich funguje. Pro jednoduchost je každou funkci možné zakreslit
jako diagram ve tvaru definičního oboru, kde se místo původních prvků na stejná
místa doplní hodnota, na kterou se ten který prvek zobrazí.

![Uspořádání na Unit_⊥_ → Bool_⊥_]($root/images/unit-bool.png)

Pro jiný typ funkcí to může dopadnout třeba tak, jako na obrázku níž.

![Bool_⊥_ → Bool_⊥_]($root/images/bool-bool.png)

## Striktní funkce

Striktní funkce je taková funkce, která nejdříve vyhodnotí svůj argument a
potom s ním teprve něco počítá. Pokud je tedy argument nedefinovaný, nemůže být
definovaný ani výsledek výpočtu. Pro striktní funkci tedy platí **f ⊥ = ⊥**.

## Odkazy

1. [Hussling Haskell types into Hasse diagrams](http://blog.ezyang.com/2010/12/hussling-haskell-types-into-hasse-diagrams/)
2. [Gin and monotonic](http://blog.ezyang.com/2010/12/gin-and-monotonic/)
3. [Getting a fix on fixpoints](http://blog.ezyang.com/2010/12/getting-a-fix-on-fixpoints/)

