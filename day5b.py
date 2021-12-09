import math


f = open("in5.txt").readlines()
lines = [[[int(i) for i in c.split(",")] for c in l.strip().split(" -> ")] for l in f]

p = {}
for [a, b], [c, d] in lines:
    if a == c or b == d:
        for x in range(min(a, c), max(a, c) + 1):
            for y in range(min(b, d), max(b, d) + 1):
                p[(x, y)] = p.get((x, y), 0) + 1
    else:
        for i in range(abs(a - c) + 1):
            q = (a + math.copysign(i, c - a), b + math.copysign(i, d - b))
            p[q] = p.get(q, 0) + 1

print(sum(c > 1 for c in p.values()))
