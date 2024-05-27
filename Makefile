##
## EPITECH PROJECT, 2024
## imageCompressor
## File description:
## Makefile
##

BINARY_PATH := $(shell stack path --local-install-root)
NAME		= mypandoc

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

cstyle: fclean
	cstyle

re : fclean all

.PHONY : all clean fclean re
