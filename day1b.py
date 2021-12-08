f = [int(i) for i in open("in1.txt").readlines()]
f = [sum(f[i:i + 3]) for i in range(0, len(f) - 2)]
print(sum(b > a for a, b in zip(f, f[1:])))
