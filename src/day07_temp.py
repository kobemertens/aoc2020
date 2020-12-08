from copy import deepcopy

with open('data.txt', 'r') as f:
    lines = f.readlines()

d = {}

for line in lines:
    first, last = line.split('bags contain')
    first = first.strip()
    last = last.strip().split(',')
    last = [alast.strip().split(' ') for alast in last]
    last = [' '.join(alast[1:-1]) for alast in last]
    d[first] = last


old = set()
collection = {'shiny gold'}

while collection != old:
    old = deepcopy(collection)
    for key, value in d.items():
        if value == ['no other']:
            continue
        for bag in value:
            if bag in collection:
                collection.add(key)

print(len(collection))

d = {}

for line in lines:
    first, last = line.split('bags contain')
    first = first.strip()
    last = last.strip().split(',')
    if len(last) == 1 and last[0] == 'no other bags.':
        continue
    last = [alast.strip().split(' ') for alast in last]
    last = [(int(alast[0]), ' '.join(alast[1:-1])) for alast in last if alast[0] != 'no']
    d[first] = last

def calculate_bag_amount(key, d):
    s = 0
    for amount, bag in d[key]:
        if bag not in d:
            s += amount
        else:
            s += amount + amount*calculate_bag_amount(bag, d)
    return s

print(calculate_bag_amount('shiny gold', d))