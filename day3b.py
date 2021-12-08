def solve(f, bitlen, keep):
    for b in range(bitlen):
        f = [n for n in f if (n[b] == keep if sum(int(n[b]) for n in f) >= len(f) / 2 else n[b] != keep)]
        if len(f) == 1:
            return int(f[0], 2)


f = open("in3.txt").readlines()
bitlen = len(f[0]) - 1
print(solve(f, bitlen, "1") * solve(f, bitlen, "0"))
