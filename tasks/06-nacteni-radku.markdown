---
title: Načtení celého řádku
category: vstup a výstup
...

Standard C99 neobsahuje žádnou funkci, která by načetla ze souboru řádek
libovolné délky. Napište funkci, která ze zadaného vstupu načte jeden celý
řádek (tj. všechny znaky až po '\n') a uloží je do dynamicky alokovaného pole.
Nejprve zkuste použít funkci `fgetc()` a načítat po jednotlivých znacích, potom
svou funkci upravte tak, aby volala `fgets()`.

<!-- SECTION -->

Norma POSIX definuje funkci `ssize_t getline(char **lineptr, size_t *n, FILE
*stream);` První argument je ukazatel na pole znaků, kam se řádek načte, druhý
je ukazatel na číselnou proměnnou obsahující délku pole v prvním argumentu a
třetí je proud, odkud se budou data načítat. Návratová hodnota určuje, kolik se
načetlo znaků, případně při chybě se vrátí -1.

To je velice praktické, pokud potřebujete postupně načítat řádky, protože to
ušetří hodně alokací. Vy ale při každém volání alokujte nové pole. Až vám to
bude fungovat, zkuste si ale naprogramovat i tuto lepší metodu.
