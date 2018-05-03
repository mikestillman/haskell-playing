setup:
	stack init
	stack setup
	stack build

distclean:
	rm -rf .stack-work stack.yaml haskell-playing.cabal
