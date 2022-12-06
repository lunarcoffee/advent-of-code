f = [[10, *[int(i) for i in r.strip()], 10] for r in open("in9.txt").readlines()]
f = [[10] * len(f[0])] + f + [[10] * len(f[0])]
s = [a for i in range(len(f)) for j in range(len(f[0])) if (a := f[i][j]) < 9 and 
     a < f[i - 1][j] and a < f[i + 1][j] and a < f[i][j - 1] and a < f[i][j + 1]]
print(sum(s) + len(s))
