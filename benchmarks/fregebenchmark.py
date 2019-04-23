import os
import shutil
import subprocess
import string
import tempfile
import pathlib

import jmhbenchmark


class FregeBenchmark(jmhbenchmark.JMHBenchmark):
    def __init__(self, name, source_path, compiler_args=None):
        if compiler_args is None:
            compiler_args = []
        source_path = pathlib.Path(source_path)

        super().__init__(name, "frege.bench", "Prog")
        self._source_path = source_path
        self._compiler_args = compiler_args
        self._temp_dir = None
        self._frege_jar_path = pathlib.Path.cwd() / "fregec.jar"

    def run(self):
        with tempfile.TemporaryDirectory() as d:
            self._temp_dir = pathlib.Path(d)
            self._compile()
            self._execute_bench(self._temp_dir)

    def _compile(self):
        self._run_frege_compiler()
        self._write_benchmark_main(self._temp_dir)
        self._build_benchmark(self._temp_dir)

    def _get_classpath(self):
        return [str(self._frege_jar_path), str(self._temp_dir)]

    def _run_frege_compiler(self):
        original_dir = pathlib.Path.cwd()
        # Build the source program
        args = (
            [
                "java",
                "-jar",
                str(self._frege_jar_path),
                # Build dir
                "-d",
                str(self._temp_dir),
            ]
            + self._compiler_args
            + [str(self._source_path)]
        )
        try:
            subprocess.check_output(args)
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise