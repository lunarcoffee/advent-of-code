f = [int(a) for a in open("in6.txt").read().split(",")]
for _ in range(80):
    f = [i for l in [[a - 1] if a else [6, 8] for a in f] for i in l]

print(len(list(f)))
