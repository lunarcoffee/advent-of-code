f = [i.split() for i in open("in2.txt").readlines()]
f = [(a, int(b)) for a, b in f]

x = sum(n for i, n in f if i[0] == "f")
y = sum(n * ((i[0] == "d") * 2 - 1) for i, n in f if i[0] != "f")
print(x * y)
