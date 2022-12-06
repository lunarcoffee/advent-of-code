f = {tuple(int(i) for i in c.split(",")) for c in open("in13.txt").readlines() if "," in c}

a, n = open("in13.txt").readlines()[len(f) + 1][11:].split("=")
for x, y in f.copy():
    if locals()[a] > int(n):
        f.remove((x, y))
        f.add((x - 2 * (x - int(n)) if a == "x" else x, y - 2 * (y - int(n)) if a == "y" else y))

print(len(f))
