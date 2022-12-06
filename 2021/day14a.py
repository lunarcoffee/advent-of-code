f = open("in14.txt").readlines()
p, rs = f[0].strip(), dict(tuple(r.strip().split(" -> ")) for r in f[2:])

for _ in range(10):
    np = p[0]
    for p in [p[i:i + 2] for i in range(len(p) - 1)]:
        np += rs.get(p, "") + p[1]
    p = np

print(max(c := [p.count(i) for i in set(p)]) - min(c))
