config:
	ghc --make Multiswitch.hs -o configgen && ./configgen 10

test:
	ghc --make Multiswitch.hs -o configgen && (./configgen 10 > monitor.p4app/p4app.json)
