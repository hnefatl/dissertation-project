#!/usr/bin/env python3.7

import sys

import benchmark
import jhaskellbenchmark

benchmarks = {}
def add_benchmark(b):
    if b.name in benchmarks:
        raise ValueError(f"Duplicate benchmark name: {b.name}")
    benchmarks[b.name] = b


add_benchmark(jhaskellbenchmark.JHaskellBenchmark("fibonacci", "programs/fibonacci.hs"))
add_benchmark(jhaskellbenchmark.JHaskellBenchmark("factorial", "programs/factorial.hs"))
add_benchmark(jhaskellbenchmark.JHaskellBenchmark("sort", "programs/sort.hs"))

to_run = sys.argv[1:]
if to_run == []:
    to_run = benchmarks.keys()

for bench_name in to_run:
    benchmark = benchmarks[bench_name]
    print(benchmark.name)
    if hasattr(benchmark, "__enter__"):
        with benchmark as b:
            b.run()
    else:
        benchmark.run()
