SHELL=/bin/sh
CC=ghc
COMPILER_RES=brambilla

default:
	$(CC) -package parsec -o $(COMPILER_RES) ./app/Main.hs

repl:
	make && ./$(COMPILER_RES)