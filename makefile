all: build

build:
	ghc --make bf

run:
	runhaskell bf.hs
