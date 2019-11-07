/*
** EPITECH PROJECT, 2019
** My_printf_2019
** File description:
** My_printf_h
*/

#ifndef MY_PRINTF_H
#define MY_PRINTF_H

void print_char(va_list my_list_of_of_args, char c, int index);
void (*print_char_ptr)(va_list, char, int);
void pr_char(va_list my_list_of_args, char c, int index);
void my_print(va_list my_list_of_args, char c, int index);
void pr_dec(va_list my_list_of_args, char c, int index);
int my_printf(const char *format, ...);



typedef void (*FN)(va_list, char, int);
typedef struct match
{
    char letter;
    FN func;
} match_t;

#endif /* !MY_PRINTF_H */
