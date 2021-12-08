f = [int(i) for i in open("in1.txt").readlines()]
print(sum(b > a for a, b in zip(f, f[1:])))
