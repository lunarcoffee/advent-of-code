f = sorted(int(i) for i in open("in7.txt").read().split(","))
print(sum(abs(x - f[len(f) // 2]) for x in f))
