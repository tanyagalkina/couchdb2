/*
** EPITECH PROJECT, 2019
** CPool_2019
** File description:
** My_putstr
*/

#include "./include/my.h"

int my_putstr(char const *str)
{
    int n = 0;

    while (*(str + n) != '\0'){
        my_putchar(*(str + n));
        ++n;
    }
    return ('*');
}