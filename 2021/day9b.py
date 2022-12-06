def descend(i, j):
    for x, y in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
        if f[i][j] > f[x][y]:
            return descend(x, y)
    s[i, j] += 1


f = [[9, *[int(i) for i in r.strip()], 9] for r in open("in9.txt").readlines()]
f = (p := [[9] * len(f[0])]) + f + p
s = {(i, j): 0 for i in range(len(f)) for j in range(len(f[0]))}
[descend(i, j) for i in range(len(f)) for j in range(len(f[0])) if f[i][j] < 9]

*_, a, b, c = sorted(s.values())
print(a * b * c)
