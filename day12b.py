def search(f, n, b):
    if n == "end":
        return paths.add(tuple(path))

    path.append(n)
    if n.isupper():
        [search(f, list(e - {n})[0], b) for e in f if n in e]
    else:
        if not b and n != "start":
            [search(f, list(e - {n})[0], True) for e in f if n in e]
        [search([e for e in f if n not in e], list(e - {n})[0], b) for e in f if n in e]
    path.pop()


path, paths = [], set()
search([set(e.strip().split("-")) for e in open("in12.txt").readlines()], "start", False)
print(len(paths))
