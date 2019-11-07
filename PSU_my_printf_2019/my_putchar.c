/*
** EPITECH PROJECT, 2019
** CPool_2019
** File description:
** My_putchar
*/

#include "./include/my.h"

void my_putchar(char c)
{
    write(1, &c, 1);
}