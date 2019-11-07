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
#include "./include/my_printf.h"

void pr_char(va_list my_list_of_args, char c, int index)
{
    my_putchar(va_arg(my_list_of_args, int));
}

void my_print(va_list my_list_of_args, char c, int index)
{
    my_putstr(va_arg(my_list_of_args, char *));
    my_putchar(10);
  
}

void pr_dec(va_list my_list_of_args, char c, int index)
{
    my_put_nbr(va_arg(my_list_of_args, int));
    my_putchar(10);
}
    
int disp_stdarg (char *s, ... )
{
    va_list my_list_of_args;
    int nb = my_strlen(s);
    
    va_start(my_list_of_args, s);
    match_t print_str;
    print_str.func = my_print;
    print_str.letter = 's';
    match_t print_dec;
    print_dec.func = pr_dec;
    print_dec.letter = 'i';
    match_t print_char;
    print_char.func = pr_char;
    print_char.letter = 'c';
    match_t func_match_array[3] = {print_str, print_dec, print_char};

    int i = 0;
    int j = 0;
    while (i < nb) {
        j = 0;
        while (j < 3) {
            
            if (func_match_array[j].letter == s[i])
                func_match_array[j].func(my_list_of_args, s[i], i);
            j = j + 1;
        }
        i = i + 1;
    }
    //func_match_array[1].func(my_list_of_args, 'X', 88);
    va_end(my_list_of_args);
    return (0);
}

int main(void)
{
    disp_stdarg("ssiicc", "HALLO!!", "ca va ?", 6, -6, 'X', 'G');
    return (0);
}
    
