#Haskell compiler
CC=ghc
Flags=--make

all:
	mkdir bin
	$(CC) $(Flags) src/exec -o ./bin/rshell

rshell:
	mkdir bin
	$(CC) $(Flags) src/exec -o ./bin/rshell
