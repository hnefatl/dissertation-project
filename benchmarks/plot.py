#!/usr/bin/env python3

import matplotlib.pyplot as plt
import re
import numpy as np
import collections
import xml.etree.ElementTree as ET

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
        "Mine_l": "results/factorial_mine_l",
        "Mine_t": "results/factorial_mine_t",
        "Mine_u": "results/factorial_mine_u",
        "Mine_l_t": "results/factorial_mine_l_t",
        "Mine_l_u": "results/factorial_mine_l_u",
        "Mine_t_u": "results/factorial_mine_t_u",
        "Mine_l_t_u": "results/factorial_mine_l_t_u",
        "Mine (opt)": "results/factorial_mine_l_t_u",
        "Frege": "results/factorial_frege",
        "Eta": "results/factorial_eta",
    },
    "fibonacci": {
        "Mine": "results/fibonacci_mine",
        "Mine_l": "results/fibonacci_mine_l",
        "Mine_t": "results/fibonacci_mine_t",
        "Mine_u": "results/fibonacci_mine_u",
        "Mine_l_t": "results/fibonacci_mine_l_t",
        "Mine_l_u": "results/fibonacci_mine_l_u",
        "Mine_t_u": "results/fibonacci_mine_t_u",
        "Mine_l_t_u": "results/fibonacci_mine_l_t_u",
        "Mine (opt)": "results/fibonacci_mine_l_t_u",
        "Frege": "results/fibonacci_frege",
        "Eta": "results/fibonacci_eta",
    },
    "mergesort": {
        "Mine": "results/mergesort_mine",
        "Mine_l": "results/mergesort_mine_l",
        "Mine_t": "results/mergesort_mine_t",
        "Mine_u": "results/mergesort_mine_u",
        "Mine_l_t": "results/mergesort_mine_l_t",
        "Mine_l_u": "results/mergesort_mine_l_u",
        "Mine_t_u": "results/mergesort_mine_t_u",
        "Mine_l_t_u": "results/mergesort_mine_l_t_u",
        "Mine (opt)": "results/mergesort_mine_l_t_u",
        "Frege": "results/mergesort_frege",
        "Eta": "results/mergesort_eta",
    },
    "ackermann": {
        "Mine": "results/ackermann_mine",
        "Mine_l": "results/ackermann_mine_l",
        "Mine_t": "results/ackermann_mine_t",
        "Mine_u": "results/ackermann_mine_u",
        "Mine_l_t": "results/ackermann_mine_l_t",
        "Mine_l_u": "results/ackermann_mine_l_u",
        "Mine_t_u": "results/ackermann_mine_t_u",
        "Mine_l_t_u": "results/ackermann_mine_l_t_u",
        "Mine (opt)": "results/ackermann_mine_l_t_u",
        "Frege": "results/ackermann_frege",
        "Eta": "results/ackermann_eta",
    },
}

COLOURS = ["blue", "red", "green", "yellow"]


results = collections.defaultdict(dict)
for bench, impls in benches.items():
    for impl, result_path in impls.items():
        with open(result_path, "rb") as f:
            results[bench][impl] = parse_results(f.read().decode())


fst = lambda x: x[0]
snd = lambda x: x[1]


def texprep(s):
    if isinstance(s, str):
        return s.replace("_", "\\_").replace("<", "\\(<\\)").replace(">", "\\(>\\)")
    elif isinstance(s, list):
        return list(map(texprep, s))
    elif isinstance(s, collections.Iterable):
        return list(map(texprep, s))
    else:
        raise ValueError


def render_fig(name, save=True):
    if save:
        plt.savefig(fname="../../dissertation/graphs/" + name, format="pdf")
    else:
        plt.show()


# TODO(kc506): Include `min_time` measurement?
def perf_by_compiler(subplot=True):
    benchmarks = sorted(list(benches.keys()))
    impls = sorted(["Mine", "Mine (opt)", "Frege", "Eta"])

    if subplot:
        fig, axes = plt.subplots(2, 3)
        axes[0][0].set_ylabel("Runtime (ms)")
    for plot_num, benchmark in enumerate(benchmarks):
        if subplot:
            ax = axes[plot_num // 3][plot_num % 3]
        else:
            fig, ax = plt.subplots()
            ax.set_ylabel("Runtime (ms)")

        quartiles = []
        for impl in impls:
            result = results[benchmark][impl]
            quartiles.append((result["lower_quartile"], result["mid_quartile"], result["upper_quartile"]))
        lows, medians, highs = map(np.array, zip(*quartiles))
        errors = [medians - lows, highs - medians]

        if subplot:
            ax.set_title("\\texttt{" + benchmark + "}")
        else:
            ax.set_title("Performance of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=impls, height=medians, yerr=errors, color=GREY, capsize=3)
        ax.tick_params(axis="x", rotation=40)
        ax.set_ylim(bottom=0)
        plt.tight_layout()
        if not subplot:
            ax.set_ylabel("Runtime (ms)")
            render_fig("perf_{}.pdf".format(benchmark).lower())
            plt.close(fig)
    if subplot:
        render_fig("perf.pdf")
        plt.close(fig)


def executable_size_by_compiler(subplot=True):
    benchmarks = sorted(list(benches.keys()))
    impls = sorted(["Mine", "Mine (opt)", "Frege", "Eta"])

    if subplot:
        fig, axes = plt.subplots(2, 3)
        axes[0][0].set_ylabel("Compiled size (bytes)")
    for plot_num, benchmark in enumerate(benchmarks):
        if subplot:
            ax = axes[plot_num // 3][plot_num % 3]
        else:
            fig, ax = plt.subplots()
            ax.set_ylabel("Compiled size (bytes)")

        sizes = []
        for impl in impls:
            result = results[benchmark][impl]
            sizes.append(result["size"])

        if subplot:
            ax.set_title("\\texttt{" + benchmark + "}")
        else:
            ax.set_title("Compiled size of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=impls, height=sizes, color=GREY)
        ax.tick_params(axis="x", rotation=40)
        ax.set_ylim(bottom=0)
        plt.ylabel("Compiled size (bytes)")
        plt.tight_layout()
        if not subplot:
            ax.set_ylabel("Compiled size (bytes)")
            render_fig("size_{}.pdf".format(benchmark).lower())
            plt.close(fig)
    if subplot:
        render_fig("size.pdf")
        plt.close(fig)


def compilation_time_by_compiler(subplot=True):
    benchmarks = sorted(list(benches.keys()))
    impls = sorted(["Mine", "Mine (opt)", "Frege", "Eta"])

    if subplot:
        fig, axes = plt.subplots(2, 3)
        axes[0][0].set_ylabel("Compilation time (ms)")
    for plot_num, benchmark in enumerate(benchmarks):
        if subplot:
            ax = axes[plot_num // 3][plot_num % 3]
        else:
            fig, ax = plt.subplots()
            ax.set_ylabel("Compilation time (ms)")

        times = []
        bottoms = []
        top_colours = []
        bottom_colours = []
        for impl in impls:
            result = results[benchmark][impl]
            if impl in {"Mine", "Mine (opt)"}:
                # We have timing information for with/without disk writes
                # Pick the pair of entries with minimum sum time. Don't take min of each list as then we might get times
                # from different runs
                time, bottom = min(zip(result["times_no_jar"], result["times"]), key=lambda x: x[0] + x[1])
                times.append(time)
                bottoms.append(bottom)
                top_colours.append(DARKGREY)
                bottom_colours.append(GREY)
            else:
                times.append(min(result["times"]))
                bottoms.append(0)
                top_colours.append(GREY)
                bottom_colours.append(DARKGREY)

        if subplot:
            ax.set_title("\\texttt{" + benchmark + "}")
        else:
            ax.set_title("Minimum compilation time of \\texttt{" + benchmark + "} by compiler")
        ax.bar(x=impls, height=times, bottom=bottoms, color=top_colours)
        ax.bar(x=impls, height=bottoms, color=bottom_colours)
        ax.tick_params(axis="x", rotation=40)
        ax.set_ylim(bottom=0)
        plt.tight_layout()
        if not subplot:
            ax.set_ylabel("Compilation time (ms)")
            render_fig("compiler_perf_{}.pdf".format(benchmark).lower())
            plt.close(fig)
    if subplot:
        render_fig("compiler_perf.pdf")
        plt.close(fig)


def optimisation_impact(subplot=True):
    benchmarks = sorted(list(benches.keys()))
    readable_impls = {
        "Mine": "None",
        "Mine_l": "L",
        "Mine_t": "D",
        "Mine_u": "U",
        "Mine_l_t": "LD",
        "Mine_l_u": "LU",
        "Mine_t_u": "DU",
        "Mine_l_t_u": "LDU",
    }

    if subplot:
        fig, axes = plt.subplots(2, 3)
        axes[0][0].set_ylabel("Runtime (ms)")
    for plot_num, benchmark in enumerate(benchmarks):
        if subplot:
            ax = axes[plot_num // 3][plot_num % 3]
        else:
            fig, ax = plt.subplots()
            ax.set_ylabel("Runtime (ms)")

        bar_data = []
        for impl, readable in readable_impls.items():
            result = results[benchmark][impl]
            bar_data.append((readable, result["min_time"]))

        sorted_bar_data = sorted(bar_data, key=snd)  # Sort by time
        labels, heights = zip(*sorted_bar_data)

        if subplot:
            ax.set_title("\\texttt{" + benchmark + "}")
        else:
            ax.set_title("Runtime of \\texttt{" + benchmark + "} by optimisation")
        ax.bar(x=texprep(labels), height=heights, color=GREY)
        ax.tick_params(axis="x", rotation=50)
        ax.set_ylim(bottom=0.95 * min(heights))
        plt.tight_layout()
        if not subplot:
            ax.set_ylabel("Runtime (ms)")
            render_fig("perf_{}_by_opt.pdf".format(benchmark).lower())
            plt.close(fig)
    if subplot:
        render_fig("perf_by_opt.pdf")
        plt.close(fig)


# Not implemented yet as this would require being able to specify order of optimisations: currently not allowed.
# Heatmap of effects of pairwise optimisations
# def optimisation_pairwise():
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


def executable_profile(subplot=True):
    results = {
        "factorial": "results/factorial.xml",
        "fibonacci": "results/fibonacci.xml",
        "mergesort": "results/mergesort.xml",
        "ackermann": "results/ackermann.xml",
    }

    if subplot:
        fig, axes = plt.subplots(2, 3)
    for plot_num, (benchmark, result_path) in enumerate(results.items()):
        if subplot:
            ax = axes[plot_num // 3][plot_num % 3]
        else:
            fig, ax = plt.subplots(figsize=(9, 4.5))
            # plt.subplots_adjust(left=0.2)

        tree = ET.parse(result_path)

        results = []
        for row in tree.getroot().find("TableData").find("TableBody").findall("TableRow"):
            label = "\\texttt{" + row[0].text.replace("tmp.", "") + "}\nInvocations: " + row[4].text
            results.append((label, float(row[1].text.strip("%")) / 100))

        results.sort(key=lambda r: r[1], reverse=True)
        # Trim down to just the functions making up a percentage of total time inside themselves
        results = [x for x in results if x[1] > 0.03]

        labels, percentages = map(list, zip(*results))

        # Add a wedge without a special font to make up the remainder of percentages
        labels.append("Other")
        percentages.append(1 - sum(percentages))

        ax.pie(x=percentages, labels=texprep(labels))
        plt.tight_layout()
        if not subplot:
            render_fig("perf_profile_{}.pdf".format(benchmark).lower())
            plt.close(fig)
    if subplot:
        render_fig("perf_profile.pdf")
        plt.close(fig)


for subplot in [True, False]:
    perf_by_compiler(subplot)
    executable_size_by_compiler(subplot)
    compilation_time_by_compiler(subplot)
    optimisation_impact(subplot)
    executable_profile(subplot)
