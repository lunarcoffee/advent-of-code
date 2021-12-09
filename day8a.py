import itertools


f = itertools.chain(*[r.split(" | ")[1].split() for r in open("in8.txt").readlines()])
print(sum(len(s) in (2, 3, 4, 7) for s in f))
