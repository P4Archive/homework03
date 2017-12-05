config:
	stack ghc Multiswitch.hs && ./Multiswitch $(HOSTS) $(HHS) && echo "" >> tests/runtest

shuffle:
	./shuffler.py ./tests/runtest
