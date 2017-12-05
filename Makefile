config:
	stack ghc Multiswitch.hs && ./Multiswitch $(HOSTS) $(HHS)

shuffle:
	./shuffler.py ./tests/runtest
