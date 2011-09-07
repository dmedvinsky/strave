.PHONY: clean


strave: clean
	ghc -o strave-parse src/main.hs

clean:
	[ -f ./strave-parse ] && rm ./strave-parse || true
