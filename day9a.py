def lower(i, j):
    c = f[i][j]
    return c < f[i - 1][j] and c < f[i + 1][j] and c < f[i][j - 1] and c < f[i][j + 1]

f = [[10, *[int(i) for i in r.strip()], 10] for r in open("in9.txt").readlines()]
f = [[10] * len(f[0])] + f + [[10] * len(f[0])]
s = [f[i][j] for i in range(1, len(f) - 1) for j in range(1, len(f[0]) - 1) if lower(i, j)]
print(sum(s) + len(s))
