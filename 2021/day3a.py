f = open("in3.txt").readlines()
b = len(f[0]) - 1

g = sum((sum(int(i.strip()[-s - 1]) for i in f) > len(f) // 2) << s for s in range(b))
print(g * (~g & 2 ** b - 1))    
