#!/usr/bin/env python3

import matplotlib.pyplot as plt
import re
import numpy as np
import collections

from results import parse_results

# Use Latex fonts
plt.rc("text", usetex=True)
plt.rc("font", family="serif")

GREY = "#909090"
DARKGREY = "#707070"

# Bar charts of benchmark performance using different languages, grouped by benchmark
benches = {
    "factorial": {
        "Mine": "results/factorial_mine",
        "Mine (opt)": "results/factorial_mine_l_t_u",
        "Frege": "results/factorial_frege",
        "Eta": "results/factorial_eta",
    },
    "fibonacci": {
        "Mine": "results/fibonacci_mine",
        "Mine (opt)": "results/fibonacci_mine_l_t_u",
        "Frege": "results/fibonacci_frege",
        "Eta": "results/fibonacci_eta",
    },
    "mergesort": {
        "Mine": "results/mergesort_mine",
        "Mine (opt)": "results/mergesort_mine_l_t_u",
        "Frege": "results/mergesort_frege",
        "Eta": "results/mergesort_eta",
    },
}

COLOURS = ["blue", "red", "green", "yellow"]


results = collections.defaultdict(dict)
for bench, impls in benches.items():
    for impl, result_path in impls.items():
        with open(result_path, "rb") as f:
            results[bench][impl] = parse_results(f.read().decode())



def render_fig(name, save=True):
    if save:
        plt.savefig(fname="plots/" + name, format="pdf")
    else:
        plt.show()


def perf_by_compiler():
    width = 0.05
    benchmarks = sorted(list(benches.keys()))
    impls = sorted(["Mine", "Mine (opt)", "Frege", "Eta"])
    xs = 1.25 * width * np.arange(len(impls))

    for benchmark in benchmarks:
        fig, ax = plt.subplots()
        quartiles = []
        for impl in impls:
            result = results[benchmark][impl]
            quartiles.append((result["lower_quartile"], result["mid_quartile"], result["upper_quartile"]))
        lows, medians, highs = map(np.array, zip(*quartiles))
        errors = [medians - lows, highs - medians]

        ax.set_title("Performance of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=xs, height=medians, width=width, yerr=errors, color=GREY, capsize=3)
        ax.set_xticks(xs)
        ax.set_xticklabels(impls)
        ax.set_ylim(bottom=0)
        plt.ylabel("Runtime (ms)")
        render_fig("perf_{}.pdf".format(benchmark).lower())
        plt.close(fig)


def executable_size_by_compiler():
    width = 0.05
    benchmarks = sorted(list(benches.keys()))
    impls = sorted(["Mine", "Mine (opt)", "Frege", "Eta"])
    xs = 1.25 * width * np.arange(len(impls))

    for benchmark in benchmarks:
        fig, ax = plt.subplots()
        sizes = []
        for impl in impls:
            result = results[benchmark][impl]
            sizes.append(result["size"])

        ax.set_title("Compiled size of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=xs, height=sizes, width=width, color=GREY, capsize=3)
        ax.set_xticks(xs)
        ax.set_xticklabels(impls)
        ax.set_ylim(bottom=0)
        plt.ylabel("Compiled size (bytes)")
        render_fig("size_{}.pdf".format(benchmark).lower())
        plt.close(fig)


def compilation_time_by_compiler():
    width = 0.05
    benchmarks = sorted(list(benches.keys()))
    impls = sorted(["Mine", "Mine (opt)", "Frege", "Eta"])
    xs = 1.25 * width * np.arange(len(impls))

    for benchmark in benchmarks:
        fig, ax = plt.subplots()
        times = []
        bottoms = []
        top_colours = []
        bottom_colours = []
        for impl in impls:
            result = results[benchmark][impl]
            #if impl in {"Mine", "Mine (opt)"}:
            if impl in {"Mine"}:
                # We have timing information for with/without disk writes
                times.append(min(result["times_no_jar"]))
                bottoms.append(min(result["times"]))
                top_colours.append(DARKGREY)
                bottom_colours.append(GREY)
            else:
                times.append(min(result["times"]))
                bottoms.append(0)
                top_colours.append(GREY)
                bottom_colours.append(DARKGREY)

        ax.set_title("Minimum compilation time of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=xs, height=times, bottom=bottoms, width=width, color=top_colours, capsize=3)
        ax.bar(x=xs, height=bottoms, width=width, color=bottom_colours, capsize=3)
        ax.set_xticks(xs)
        ax.set_xticklabels(impls)
        ax.set_ylim(bottom=0)
        plt.ylabel("Compilation time (ms)")
        render_fig("compiler_perf_{}.pdf".format(benchmark).lower())
        plt.close(fig)


# Not implemented yet as this would require being able to specify order of optimisations: currently not allowed.
#def optimisation_impact():
#    benchmarks = sorted(list(benches.keys()))
#    impls = sorted(["Mine", "Mine (opt)", "Frege", "Eta"])
#
#    opts = [None, "l", "t", "u"]
#
#    img_data = []
#    for benchmark in benchmarks:
#        for opt1 in opts:
#            row = []
#            for opt2 in opts:
#
#                row.append()
#            img_data.append(row)
#

perf_by_compiler()
executable_size_by_compiler()
compilation_time_by_compiler()
