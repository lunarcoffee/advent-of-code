f = {tuple(int(i) for i in c.split(",")) for c in open("in13.txt").readlines() if "," in c}

for a, n in [i[11:].split("=") for i in open("in13.txt").readlines() if " " in i]:
    for x, y in f.copy():
        if locals()[a] > int(n):
            f.remove((x, y))
            f.add((x - 2 * (x - int(n)) if a == "x" else x, y - 2 * (y - int(n)) if a == "y" else y))

m, n = max(int(c[0]) for c in f), max(int(c[1]) for c in f)
for i in range(n + 1):
    for j in range(m + 1):
        print("#" if (j, i) in f else " ", end="")
    print()
