.PHONY: all

all: main.js

main.js:  $(shell find src -type f)
	elm make src/Main.elm --output=main.js
