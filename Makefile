config:
	stack ghc Multiswitch.hs && ./Multiswitch $(HOSTS) $(HHS) && ./shuffler.py ./tests/runtest

shuffle:
	./shuffler.py ./tests/runtest
