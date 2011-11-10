---
title: Sčítačka
category: vstup a výstup
...

Napište program, který načte ze vstupu dvě celá čísla a vypíše jejich součet.
Až budete umět sčítat na korektním vstupu, ošetřete případy, kdy uživatel zadá
jen žádné nebo jen jedno číslo, případně to vůbec číslo nebude.

<!-- SECTION -->

Použijte funkci `scanf`, ideálně právě jedno volání.

<!-- SECTION -->

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.c}
#include <stdio.h>
int main(void)
{
    int x, y;
    if (scanf("%d %d", &x, &y) != 2) {
        fprintf(stderr, "Spatny vstup\n");
        return 1;
    }
    printf("%d\n", x + y);
    return 0;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
