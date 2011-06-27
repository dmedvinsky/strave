statrave: clean
	ghc -tmpdir /tmp -odir /tmp -o statrave src/main.hs

run: statrave
	cat ~/.workrave/historystats | ./statrave

clean:
	[ -f ./statrave ] && rm ./statrave || true
