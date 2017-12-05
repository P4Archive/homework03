#! /usr/bin/python

from random import shuffle
import sys

items = []
with open(sys.argv[1]) as ifile:
  for line in ifile:
    items.append(line.strip())

shuffle(items)
with open(sys.argv[1], "w") as ofile:
  for item in items:
    ofile.write(item + "\n") 
