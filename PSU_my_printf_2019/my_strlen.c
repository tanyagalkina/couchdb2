/*
** EPITECH PROJECT, 2019
** CPool_Day04
** File description:
** My_strlen
*/

int my_strlen(char const *str)
{
    int n = 0;
    while (*(str + n) != '\0') {
        ++n;
    }
    return (n);
}