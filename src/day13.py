# a = [17, 37, 907, 19, 23, 29, 653, 41, 13]
# start = 1000186
# while all(start % n != 0 for n in a):
#     start += 1

# print(start, [start % n == 0 for n in a])
# print((1000194-1000186)*13)

with open("data/day13.txt") as f:
    lines = f.readlines()
print(lines)

lines = lines[1]

d = {int(num):i for i, num in enumerate(lines.split(',')) if num !="x"}

print(d)