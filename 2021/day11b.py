f = [[-65536, *[int(o) for o in r.strip()], -65536] for r in open("in11.txt").readlines()]
f = (p := [[-65536] * len(f[0])]) + f + p

s = 1
while True:
    f = [[o + 1 for o in r] for r in f]
    fl = 0

    c = True
    while c:
        c = False
        for i in range(len(f)):
            for j in range(len(f[0])):
                if f[i][j] > 9:
                    n = ((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
                    for x, y in n:
                        if f[i + x][j + y] >= 0:
                            f[i + x][j + y] += 1
                    f[i][j] = -1
                    fl += 1
                    c = True

    f = [[0 if o == -1 else o for o in r] for r in f]
    
    if fl == (len(f) - 2) * (len(f[0]) - 2):
        print(s)
        break
    s += 1
