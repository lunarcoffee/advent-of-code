f = [i.split() for i in open("in2.txt").readlines()]
f = [(a, int(b)) for a, b in f]

x, y = 0, 0
for i, n in f:
    if i[0] == "f":
        x += n
    elif i[0] == "u":
        y -= n
    else:
        y += n

print(x * y)
