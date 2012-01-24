---
title: Fibonacciho posloupnost
category: algoritmy
---

Napište funkci `int fibonacci(int n)`, která vrátí `n`-té číslo [Fibonacciho
posloupnosti](http://cs.wikipedia.org/wiki/Fibonacciho_posloupnost). Nejprve
vytvořte rekurzivní variantu a zkuste ji spustit pro různě velká `n` (klidně
i 50 ☺). Teď
funkci přepište tak, aby žádnou rekurzi nepoužívala.

<!-- SECTION -->

Když nemáte rekurzi, těžko můžete počítat dvě menší hodnoty, takže asi vám
nezbyde nic jiného než je počítat zezdola. První dva prvky jsou jasné a z nich
jde spočítat třetí, atd. Bude tedy potřebovat cyklus a alespoň dvě proměnné.
