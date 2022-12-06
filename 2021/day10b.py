import functools


def completion(l):
    s = []
    for c in l:
        if c not in otc:
            if otc[s[-1]] != c:
                return ""
            s.pop()
        else:
            s.append(c)
    return "".join(otc[c] for c in s[::-1])


otc = {k: v for k, v in zip("([{<", ")]}>")}
f = [functools.reduce(lambda s, c: 5 * s + " )]}>".index(c), c, 0) 
     for l in open("in10.txt").readlines() if (c := completion(l.strip()))]
print(sorted(f)[len(f) // 2])
