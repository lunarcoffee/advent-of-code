f = [int(a) for a in open("in6.txt").read().split(",")]
c = [f.count(i) for i in range(9)]
for _ in range(256):
    new = c[0]
    c = c[1:] + [c[0]]
    c[6] += new

print(sum(c))
