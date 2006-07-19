.PHONY: haddock haddock-clean

haddock:
	find src/Database driver-*/Database -name '*.hs' | xargs haddock -h -o doc/api

haddock-clean:
	-rm -f doc/api/*