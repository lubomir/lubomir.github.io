---
title: Sčítačka
category: vstup a výstup
...

Napište program, který načte ze vstupu dvě celá čísla a vypíše jejich součet.

<!-- SECTION -->

Použijte funkci `scanf`, ideálně právě jedno volání.

<!-- SECTION -->

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.c}
#include <stdio.h>
int main(void)
{
    int x, y;
    scanf("%d %d\n", &x, &y);
    printf("%d\n", x + y);
    return 0;
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
