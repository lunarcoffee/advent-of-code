import bisect


f = sorted(int(i) for i in open("in7.txt").read().split(","))
cost = lambda i: sum(sum(range(1, abs(x - i) + 1)) for x in f)
print(cost(bisect.bisect_left(range(len(f) - 1), True, key=lambda i: cost(i) < cost(i + 1))))
