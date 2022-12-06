total = 0
for r in open("in8.txt").readlines():
    r, p = [s.split() for s in r.split(" | ")]
    ft = lambda l: [frozenset(d) for d in r if len(d) == l]

    m = {i: ft(n)[0] for i, n in ((1, 2), (4, 4), (7, 3), (8, 7))}
    m[6], = [d for d in ft(6) if m[8] - d & m[1]]
    m[9], = [d for d in ft(6) if m[4] < d]
    m[0], = set(ft(6)) - {m[6], m[9]}
    m[5], = [d for d in ft(5) if d < m[6]]
    m[3], = [d for d in ft(5) if m[7] < d]
    m[2], = set(ft(5)) - {m[3], m[5]}

    mr = {v: k for k, v in m.items()}
    total += int("".join(str(mr[frozenset(d)]) for d in p))

print(total)
