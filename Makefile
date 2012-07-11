PROG_PREV = ./dist/build/web-crawler/web-crawler

DIST=dist

default: build-dev

clean:
	rm -rf $(DIST)

build: 
	cabal configure
	cabal build

rebuild: clean build

run:
	$(PROG_PREV)
