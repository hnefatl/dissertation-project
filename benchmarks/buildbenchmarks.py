#!/usr/bin/env python3.7

import os
import subprocess
import string
import contextlib
import tempfile
import pathlib

import benchmark
import jhaskellbenchmark

RUNTIME_DIR = "../runtime"

BENCHMARKS = [
    jhaskellbenchmark.JHaskellBenchmark("fibonacci", "programs/fibonacci.hs")
]

for benchmark in BENCHMARKS:
    if hasattr(benchmark, "__enter__"):
        with benchmark as b:
            b.run()
    else:
        benchmark.run()
