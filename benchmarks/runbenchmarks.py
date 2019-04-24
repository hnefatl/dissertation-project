#!/usr/bin/env python3.7

import sys
import re
import itertools

import benchmark
from jhaskellbenchmark import JHaskellBenchmark
from javabenchmark import JavaBenchmark
from fregebenchmark import FregeBenchmark


def powerset(s):
    return itertools.chain.from_iterable(itertools.combinations(s, r) for r in range(len(s)+1))


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
add_benchmark(FregeBenchmark("fibonacci_frege", "programs/fibonacci.fr"))
add_benchmark(FregeBenchmark("factorial_frege", "programs/factorial.fr"))
add_benchmark(FregeBenchmark("mergesort_frege", "programs/mergesort.fr"))
add_benchmark(JavaBenchmark("factorial_java", "factorial", "programs/Factorial.java"))
add_benchmark(JavaBenchmark("fibonacci_java", "fibonacci", "programs/Fibonacci.java"))

if len(sys.argv) > 1:
    to_run = [b for b in benchmarks.keys() if any(re.fullmatch(pat, b) for pat in sys.argv[1:])]
else:
    to_run = list(benchmarks.keys())

for bench_name in to_run:
    with benchmarks[bench_name] as benchmark:
        print(benchmark.description)
        benchmark.execute()
