#!/usr/bin/env python3.7

import sys

import benchmark
from jhaskellbenchmark import JHaskellBenchmark
from javabenchmark import JavaBenchmark
from fregebenchmark import FregeBenchmark

benchmarks = {}


def add_benchmark(b):
    if b.name in benchmarks:
        raise ValueError(f"Duplicate benchmark name: {b.name}")
    benchmarks[b.name] = b


add_benchmark(JHaskellBenchmark("fibonacci", "programs/fibonacci.hs"))
add_benchmark(JHaskellBenchmark("factorial", "programs/factorial.hs"))
add_benchmark(JHaskellBenchmark("mergesort", "programs/mergesort.hs"))
add_benchmark(FregeBenchmark("fibonacci_frege", "programs/fibonacci.fr"))
add_benchmark(FregeBenchmark("factorial_frege", "programs/factorial.fr"))
add_benchmark(FregeBenchmark("mergesort_frege", "programs/mergesort.fr"))
add_benchmark(JavaBenchmark("factorial_java", "factorial", "programs/Factorial.java"))
add_benchmark(JavaBenchmark("fibonacci_java", "fibonacci", "programs/Fibonacci.java"))

to_run = sys.argv[1:]
if to_run == []:
    to_run = benchmarks.keys()

for bench_name in to_run:
    with benchmarks[bench_name] as benchmark:
        print(benchmark.description)
        benchmark.execute()
