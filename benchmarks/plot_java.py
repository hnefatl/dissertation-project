#!/usr/bin/env python3.7

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import re

# Use Latex fonts
plt.rc("text", usetex=True)
plt.rc("font", family="serif")

class Result:
    def __init__(self, name, histogram, percentiles):
        self._name = name
        self._histogram = histogram
        self._percentiles = percentiles

    def __str__(self):
        s = [f"{self._name}:"]
        s.append("Histogram:")
        for range, val in self._histogram.items():
            s.append(f"[{range[0]}, {range[1]}) = {val}")
        s.append("Percentiles:")
        for percentile, val in self._percentiles.items():
            s.append(f"p({percentile}) = {val}")
        return "\n".join(s)


def parse_results(name, file):
    # Skip to the histogram data
    for _ in range(4):
        file.readline()

    histogram = {}
    histogram_match = re.compile(r'\[([\s\d\.]+),([\s\d\.]+)\) = ([\d\.]+)')
    for line in file:
        match = histogram_match.match(line.strip())
        if not match:
            break
        histogram[(match.group(1), match.group(2))] = match.group(3)

    # Skip to the percentile data
    file.readline()

    percentiles = {}
    percentile_match = re.compile(r'\s*p\(([\d.]+)\) =\s*([\d.]+) ms/op')
    for line in file:
        match = percentile_match.match(line.strip())
        if not match:
            break
        percentiles[match.group(1)] = match.group(2)

    return Result(name, histogram, percentiles)

results_regex = re.compile(r'Result "benchmark\.Main\.(.+)"')
results = []
with open("java_results", "r", encoding="utf-8") as f:
    for line in f:
        match = results_regex.match(line)
        if match:
            results.append(parse_results(match.group(1), f))
    
for result in results:
    print(result)
    print()

#plt.show()