def first_corrupt(l):
    s = []
    for c in l:
        if c in cto:
            if s[-1] != cto[c]:
                return c
            s.pop()
        else:
            s.append(c)


cto = {k: v for k, v in zip(")]}>", "([{<")}
f = [c for l in open("in10.txt").readlines() if (c := first_corrupt(l.strip()))]
print(sum({")": 3, "]": 57, "}": 1197, ">": 25137}[i] for i in f))
