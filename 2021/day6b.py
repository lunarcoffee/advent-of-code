f = [int(a) for a in open("in6.txt").read().split(",")]
c = [f.count(i) for i in range(9)]
for _ in range(256):
    c = [*c[1:7], c[7] + c[0], c[8], c[0]]

print(sum(c))
