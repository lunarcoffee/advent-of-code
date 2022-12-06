print(sum(len(s) in (2, 3, 4, 7) for s in 
    [d for l in [r.split(" | ")[1].split() for r in open("in8.txt").readlines()] for d in l]))
