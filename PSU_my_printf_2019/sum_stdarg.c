/*
** EPITECH PROJECT, 2019
** Printf_bootstrap
** File description:
** Sum_stdarg
*/

#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>
#include "./include/my.h"

int      sum_stdarg( int i, int nb, ...)
{
    va_list my_list_of_args;
    int sum1 = 0;
    int sum2 = 0;
    
    va_start(my_list_of_args, nb);
    if (i == 0) {
        for (int j = 0; j < nb; j = j + 1)
        sum1 = sum1 + va_arg(my_list_of_args, int);
        return (sum1);
    } else {
        for (int k = 0; k < nb; k = k + 1)
            sum2 = sum2 + my_strlen(va_arg(my_list_of_args, char *));
            return (sum2);
    }
}