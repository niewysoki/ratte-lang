all:
		cabal build

install:
		cabal install --install-method=copy --installdir=./ --overwrite-policy=always

clean:
		cabal clean
		rm interpreter