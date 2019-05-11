#!/usr/bin/env python3.7

import sys
import re
import itertools
import argparse

import benchmark
from jhaskellbenchmark import JHaskellBenchmark
from javabenchmark import JavaBenchmark
from fregebenchmark import FregeBenchmark
from etabenchmark import EtaBenchmark


def powerset(s):
    return itertools.chain.from_iterable(itertools.combinations(s, r) for r in range(len(s) + 1))


benchmarks = {}


def add_benchmark(b):
    if b.name in benchmarks:
        raise ValueError(f"Duplicate benchmark name: {b.name}")
    benchmarks[b.name] = b


for opts in powerset(["l", "t", "u"]):
    sorted_opts = sorted(opts)
    args = ["-" + s for s in sorted_opts]
    name_suffix = "".join(["_" + s for s in sorted_opts])
    add_benchmark(JHaskellBenchmark(f"fibonacci_mine{name_suffix}", "programs/fibonacci.hs", compiler_args=args))
    add_benchmark(JHaskellBenchmark(f"factorial_mine{name_suffix}", "programs/factorial.hs", compiler_args=args))
    add_benchmark(JHaskellBenchmark(f"mergesort_mine{name_suffix}", "programs/mergesort.hs", compiler_args=args))
    add_benchmark(JHaskellBenchmark(f"ackermann_mine{name_suffix}", "programs/ackermann.hs", compiler_args=args))
    add_benchmark(JHaskellBenchmark(f"hanoi_mine{name_suffix}", "programs/hanoi.hs", compiler_args=args))
    add_benchmark(JHaskellBenchmark(f"lists_mine{name_suffix}", "programs/lists.hs", compiler_args=args))
add_benchmark(FregeBenchmark("fibonacci_frege", "programs/fibonacci.fr"))
add_benchmark(FregeBenchmark("factorial_frege", "programs/factorial.fr"))
add_benchmark(FregeBenchmark("mergesort_frege", "programs/mergesort.fr"))
add_benchmark(FregeBenchmark("ackermann_frege", "programs/ackermann.fr"))
add_benchmark(FregeBenchmark("hanoi_frege", "programs/hanoi.fr"))
add_benchmark(FregeBenchmark("lists_frege", "programs/lists.fr"))
for suffix, args in [("", []), ("_opt", ["-O3"])]:
    add_benchmark(EtaBenchmark(f"fibonacci_eta{suffix}", "programs/fibonacci.eta", compiler_args=args))
    add_benchmark(EtaBenchmark(f"factorial_eta{suffix}", "programs/factorial.eta", compiler_args=args))
    add_benchmark(EtaBenchmark(f"mergesort_eta{suffix}", "programs/mergesort.eta", compiler_args=args))
    add_benchmark(EtaBenchmark(f"ackermann_eta{suffix}", "programs/ackermann.eta", compiler_args=args))
    add_benchmark(EtaBenchmark(f"hanoi_eta{suffix}", "programs/hanoi.eta", compiler_args=args))
    add_benchmark(EtaBenchmark(f"lists_eta{suffix}", "programs/lists.eta", compiler_args=args))
add_benchmark(JavaBenchmark("factorial_java", "factorial", "programs/Factorial.java"))
add_benchmark(JavaBenchmark("fibonacci_java", "fibonacci", "programs/Fibonacci.java"))

parser = argparse.ArgumentParser()
parser.add_argument("patterns", nargs="*", help="Regex patterns to select benchmarks to perform")
parser.add_argument("--list-benchmarks", action="store_true", help="List all available benchmarks")
args = parser.parse_args()

if args.list_benchmarks:
    for b in benchmarks.keys():
        print(b)

to_run = [b for b in benchmarks.keys() if any(re.fullmatch(pat, b) for pat in args.patterns)]

for bench_name in to_run:
    try:
        with benchmarks[bench_name] as benchmark:
            print(benchmark.description)
            benchmark.execute()
    except Exception as e:
        print(e)
