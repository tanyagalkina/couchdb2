##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makes library
##

SRC	=	my_putchar.c			\
		my_put_nbr.c			\
		my_strlen.c			\
		my_putstr.c			\
	        myprintf.c			\

OBJ	=	$(SRC:.c=.o)

NAME	=	libmy.a

all:		$(NAME)

$(NAME):	$(OBJ)
		gcc -c $(SRC)
		ar rc $(NAME) $(OBJ)
clean:
		rm $(OBJ)

fclean:		clean
		rm $(NAME)

re:		fclean all

.PHONY:		all clean fclean re