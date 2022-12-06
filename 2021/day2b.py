f = [(a, int(b)) for a, b in [i.split() for i in open("in2.txt").readlines()]]

x, y, a = 0, 0, 0
for i, n in f:
    if i[0] == "f":
        x += n
        y += a * n
    else:
        a += n * ((i[0] == "d") * 2 - 1)

print(x * y)
