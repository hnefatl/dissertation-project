#!/usr/bin/env python3

import matplotlib.pyplot as plt
import re
import numpy as np
import collections

# Use Latex fonts
plt.rc("text", usetex=True)
plt.rc("font", family="serif")

# Bar charts of benchmark performance using different languages, grouped by benchmark
# TODO(kc506): These should really be by compiler rather than by language
benches = {
    "factorial": {"Haskell": "results/factorial", "Java": "results/factorial_java"},
    "fibonacci": {"Haskell": "results/fibonacci", "Java": "results/fibonacci_java"},
}
language_results = collections.defaultdict(list)
for bench, impls in benches.items():
    for impl, result_path in impls.items():
        with open(result_path, "rb") as f:
            low_quartile, mid_quartile, high_quartile = map(lambda line: float(line.decode()), f)
        language_results[impl].append((low_quartile, mid_quartile, high_quartile))


fix, ax = plt.subplots()
width = 0.35
loc = np.arange(len(benches))

bars = []
for language_number, (language, results) in enumerate(language_results.items()):
    lows, medians, highs = map(np.array, zip(*results))
    errors = [medians - lows, highs - medians]
    bars.append(ax.bar(x=loc + width * language_number, height=medians, width=width, yerr=errors))

ax.set_title("Benchmark performance by compiler")
ax.set_xticks(loc + width / 2)
ax.set_xticklabels(benches.keys())
ax.yaxis.set_units("ms")
ax.legend(map(lambda b: b[0], bars), language_results.keys())
ax.autoscale_view()

plt.show()
plt.clf()
plt.cla()
