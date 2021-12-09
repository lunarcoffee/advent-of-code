import itertools


f = [int(a) for a in open("in6.txt").read().split(",")]
for _ in range(80):
    f = itertools.chain(*[[a - 1] if a else [6, 8] for a in f])

print(len(list(f)))
