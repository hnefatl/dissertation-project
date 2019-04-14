#!/usr/bin/env python3.7

import os
import subprocess
import string
import contextlib
import tempfile
import pathlib

import benchmark
import jhaskellbenchmark

BENCHMARKS = [
    jhaskellbenchmark.JHaskellBenchmark("fibonacci", "programs/fibonacci.hs"),
    jhaskellbenchmark.JHaskellBenchmark("factorial", "programs/factorial.hs"),
    jhaskellbenchmark.JHaskellBenchmark("sort", "programs/sort.hs")
]

for benchmark in BENCHMARKS:
    print(benchmark.name)
    if hasattr(benchmark, "__enter__"):
        with benchmark as b:
            b.run()
    else:
        benchmark.run()
