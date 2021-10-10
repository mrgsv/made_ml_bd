#! /usr/bin/python3
import sys
import csv

count = 0
mean = 0
mean_of_sq = 0
for line in csv.reader(sys.stdin):
    try:
        price = float(line[9])
    except ValueError:
        continue
    mean = count / (count + 1) * mean + price / (count + 1)
    mean_of_sq = count / (count + 1) * mean_of_sq + (price ** 2) / (count + 1)
    count += 1

print(
    str(count), 
    str(mean), 
    str(mean_of_sq - mean ** 2), 
    sep="\t"
)
    