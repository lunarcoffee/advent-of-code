def calc(f, b, k):
    for s in range(b):
        if len(f) == 1:
            return int(f[0], 2)
        f = [n for n in f if (n[s] == k if sum(int(n[s]) for n in f) >= len(f) / 2 else n[s] != k)]


f = open("in3.txt").readlines()
b = len(f[0]) - 1
print(calc(f, b, "1") * calc(f, b, "0"))
