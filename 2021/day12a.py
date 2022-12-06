def search(f, n):
    if n == "end":
        return 1
    l = [e for e in f if n not in e] if n.islower() else f
    return sum(search(l, list(e - {n})[0]) for e in f if n in e)


f = [set(e.strip().split("-")) for e in open("in12.txt").readlines()]
print(search(f, "start"))
