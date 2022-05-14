all: build install

build:
	cabal build

install:
	cabal install --install-method=copy --installdir=./ --overwrite-policy=always

.PHONY: clean
clean:
	cabal clean
	rm interpreter