#!/usr/bin/env python3.7

import os
import subprocess

RUNTIME_DIR = "runtime"
BIN_DIR = "bins"

class Benchmark:
    def compile():
        raise NotImplementedError

    def get_import():
        raise NotImplementedError

    def get_bench_function():
        raise NotImplementedError

class JHaskellBenchmark(Benchmark):
    def __init__(self, name):
        self._package_name = name.lower()
        self._class_name = self._package_name.capitalize()

    def compile(self):
        args = [
            "compiler-exe",
            "--output-jar",
            f"{BIN_DIR}/{self._package_name}.jar",
            "--output-class",
            self._class_name,
            "--runtime-file-dir",
            RUNTIME_DIR,
            f"{self._package_name}/{self._package_name}.hs"
        ]
        try:
            output = subprocess.check_output(args).decode()
        except subprocess.CalledProcessError:
            print(output)
            raise

    def get_import(self):
        return f"import {self._package_name}.{self._class_name};"

    def get_bench_function(self):
        return f"""    @Benchmark
    @BenchmarkMode(Mode.SampleTime)
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    public void ${program}() {
        ${program}.${class}.main(args);
    }"""


def compile_benchmarks():
    os.makedirs("bins")

    for dir in os.listdir("programs"):
        os.chdir("programs")
    os.chdir("..")