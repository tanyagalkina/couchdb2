/*
** EPITECH PROJECT, 2019
** My_printf_2019
** File description:
** My-printf-start
*/

#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>
#include "./include/my.h"
#include "./include/my_printf.h"

match_t func_match_array[3] = {
    { 'c', pr_char},
    { 'd', pr_dec},
    { 's', my_print}
};

void pr_char(va_list my_list_of_args, char c, int index)
{
    my_putchar(va_arg(my_list_of_args, int));
}

void my_print(va_list my_list_of_args, char c, int index)
{
    my_putstr(va_arg(my_list_of_args, char *));
}

void pr_dec(va_list my_list_of_args, char c, int index)
{
    my_put_nbr(va_arg(my_list_of_args, int));
}

int my_printf_intern(const char *format, va_list my_arg_list)
{
    int i = 0;
    int j = 0;
    
    while (format[i] != '\0')
    {
        if (format[i] == '%') {
            j = 0;
            while (j < 3) {
                if (func_match_array[j].letter == format[i + 1])
                    func_match_array[j].func(my_arg_list, format[i + 1], i);
                j = j + 1;
            }
	    i = i + 1;
        } else
            my_putchar(format[i]);
        i = i + 1;
    }
    return (0);
}

int my_printf(const char *format, ...)
{
    va_list my_arg_list;
    int done;
    
    va_start(my_arg_list, format);
    done = my_printf_intern(format, my_arg_list);
    va_end(my_arg_list);
    return (done);
}
