#!/usr/bin/env python3

import matplotlib.pyplot as plt
import re
import numpy as np
import collections

import results

# Use Latex fonts
plt.rc("text", usetex=True)
plt.rc("font", family="serif")

# Bar charts of benchmark performance using different languages, grouped by benchmark
benches = {
    "factorial": {
        "Mine": "results/factorial_mine",
        "Frege": "results/factorial_frege",
        "Java": "results/factorial_java",
    },
    "fibonacci": {
        "Mine": "results/fibonacci_mine",
        "Frege": "results/fibonacci_frege",
        "Java": "results/fibonacci_java",
    },
    "mergesort": {"Mine": "results/mergesort_mine", "Frege": "results/mergesort_frege"},
}
compiler_results = collections.defaultdict(dict)
for bench, impls in benches.items():
    for impl, result_path in impls.items():
        with open(result_path, "rb") as f:
            compiler_results[bench][impl] = results.parse_results(f.read().decode())

COLOURS = ["blue", "red", "green", "yellow"]


def render_fig(name):
    save = True
    if save:
        plt.savefig(fname="plots/" + name, format="pdf")
    else:
        plt.show()


def perf_by_compiler():
    width = 0.15

    for benchmark, impls in sorted(compiler_results.items()):
        widths = width * np.arange(len(impls))
        fig, ax = plt.subplots()
        quartiles = []
        for impl, result in sorted(impls.items()):
            quartiles.append((result["lower_quartile"], result["mid_quartile"], result["upper_quartile"]))
        lows, medians, highs = map(np.array, zip(*quartiles))
        errors = [medians - lows, highs - medians]

        ax.set_title("Performance of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=widths, height=medians, width=width, yerr=errors, color=COLOURS)
        ax.set_xticks(widths)
        ax.set_xticklabels(sorted(impls.keys()))
        ax.set_ylim(bottom=0)
        plt.ylabel("Runtime (ms)")
        render_fig("perf_{}_{}".format(benchmark, impl).lower())
        plt.close(fig)


def executable_size_by_compiler():
    width = 0.15

    for benchmark, impls in sorted(compiler_results.items()):
        widths = width * np.arange(len(impls))
        fig, ax = plt.subplots()
        sizes = []
        for impl, result in sorted(impls.items()):
            sizes.append(result["size"])

        ax.set_title("Compiled size of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=widths, height=sizes, width=width, color=COLOURS)
        ax.set_xticks(widths)
        ax.set_xticklabels(sorted(impls.keys()))
        ax.set_ylim(bottom=0)
        plt.ylabel("Compiled size (bytes)")
        render_fig("size_{}_{}".format(benchmark, impl).lower())
        plt.close(fig)


def compilation_time_by_compiler():
    width = 0.15

    for benchmark, impls in sorted(compiler_results.items()):
        widths = width * np.arange(len(impls))
        fig, ax = plt.subplots()
        times = []
        for impl, result in sorted(impls.items()):
            times.append(min(result["times"]))

        ax.set_title("Minimum compilation time of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=widths, height=times, width=width, color=COLOURS)
        ax.set_xticks(widths)
        ax.set_xticklabels(sorted(impls.keys()))
        ax.set_ylim(bottom=0)
        plt.ylabel("Compilation time (ms)")
        render_fig("compiler_perf_{}_{}".format(benchmark, impl).lower())
        plt.close(fig)


perf_by_compiler()
executable_size_by_compiler()
compilation_time_by_compiler()
