.PHONY: clean


strave: src/main.hs
	ghc -o strave src/main.hs

clean:
	[ -f ./strave ] && rm ./strave || true
	rm ./src/*.hi ./src/*.o
