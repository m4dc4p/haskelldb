.PHONY: haddock haddock-clean

haddock:
	find src -name '*.hs' | xargs haddock -h -o doc/api

haddock-clean:
	-rm -f doc/api/*