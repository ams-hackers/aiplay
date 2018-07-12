.PHONY: all build check clean format

all: build

build:
	stack build --pedantic

check:
	hlint .

clean:
	stack clean

format:
	find . -name '*.hs' -exec hindent {} \;
