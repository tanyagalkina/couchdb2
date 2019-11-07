/*B
** EPITECH PROJECT, 2019
** CPool_Day09_2019
** File description:
** My_MY-H
*/

#ifndef MY_H
#define MY_H

#include <unistd.h>
#include <stdlib.h>

void    my_putchar(char);
int      sum_stdarg( int i, int nb, ...);
char    *add2(char *pr, char *de, int len);
char    *add(char *first, char *second);
void    end(char **result, int carry, int j);
int     disp_stdarg( char *s, ... );
int     my_isneg(int);
int     is_letter(char c);
char    *fake(char *first, char *second, char sign);
char    *sub(char *first, char *second);
int     my_isdigit(char c);
int     eval_exp(char *str);
char    *prod(char *pr, char *de);
int     eval_expr(char *str);
int     multi(int i, const char *str);
int     my_put_nbr(int);
char    *infin_add(const char *, const char *);
int     number(char **str);
int     summands(char **str);
void    my_swap(int, int);
int     my_putstr(char const *);
int     my_strlen(char const *);
int     my_getnbr(char const *);
void    my_sort_int_array(int *, int);
int     my_str_isprintable(char const *);
char    *my_strdup(char const *src);
int     my_str_isupper(char const *);
int     my_compute_power_rec(int, int);
int     my_compute_square_root(int);
char    *my_revstr(char *);
char    *my_strlowcase(char *);
int     my_showmem(char const *, int);
//char    *my_strncat(char *, char const *, int);
int     my_find_prime_sup(int);
int     my_showstr(char const *);
int     my_strncmp(char const *, char const *, int);
char    *my_strncpy(char *, char const *, int);
char    *my_strcapitalize(char *);
char    *my_strstr(char *, char const *);
int     my_is_prime(int);
char    *my_strcat(char *, char const *);
char    *my_strupcase(char *);
int     my_strcmp(char const *, char const *);
char    *my_strcpy(char *, char const *);
int     my_str_isalpha(char const *);
int     my_str_islower(char const *);
int     my_str_isnum(char const *);
char    *my_strncat(char *dest, char const *src, int nb);

#endif /* !MY_H */