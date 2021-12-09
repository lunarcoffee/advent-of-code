import re, sys


f = open("in4.txt").readlines()
boards = [[re.split(" +", r.strip()) for r in f[i:i + 5]] for i in range(2, len(f), 6)]

for m in f[0].split(","):
    boards = [[[("" if c == m else c) for c in r] for r in b] for b in boards]
    for b in boards:
        if any(not "".join(r) for r in b) or any(all(not r[c] for r in b) for c in range(5)):
            print(int(m) * sum(sum(int(c) for c in r if c) for r in b))
            sys.exit()
