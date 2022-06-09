##
## EPITECH PROJECT, 2022
## Untitled (Workspace)
## File description:
## Makefile
##

BIN = $(shell stack path --local-install-root)

NAME = wolfram

all:
	stack build
	cp $(BIN)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re
