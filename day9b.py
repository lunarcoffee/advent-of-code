def lower(i, j):
    c = f[i][j]
    return c < f[i - 1][j] and c < f[i + 1][j] and c < f[i][j - 1] and c < f[i][j + 1]


def descend(i, j):
    for x, y in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
        if f[i][j] > f[x][y]:
            return descend(x, y)
    s[(i, j)] += 1


f = [[10, *[int(i) for i in r.strip()], 10] for r in open("in9.txt").readlines()]
f = [[10] * len(f[0])] + f + [[10] * len(f[0])]
s = {(i, j): 0 for i in range(len(f)) for j in range(len(f[0]))}
[descend(i, j) for i in range(1, len(f) - 1) for j in range(1, len(f[0]) - 1) if f[i][j] < 9]

t = sorted(s.values())
print(t[-1] * t[-2] * t[-3])
