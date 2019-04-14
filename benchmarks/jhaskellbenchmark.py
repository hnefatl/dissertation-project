import os
import shutil
import subprocess
import string
import tempfile
import pathlib

import jmhbenchmark


class JHaskellBenchmark(jmhbenchmark.JMHBenchmark):
    def __init__(self, name, source_path, compiler_args=None):
        if compiler_args is None:
            compiler_args = []

        source_path = pathlib.Path(source_path)
        super().__init__(name, source_path.stem.lower(), source_path.stem.capitalize())
        self._source_path = source_path
        self._compiler_args = compiler_args
        self._temp_dir = None

    def run(self):
        with tempfile.TemporaryDirectory() as d:
            self._temp_dir = pathlib.Path(d)
            self._compile()
            self._execute_bench(self._temp_dir)

    def _compile(self):
        self._run_jhaskell_compiler()
        self._write_benchmark_main(self._temp_dir)
        self._build_benchmark(self._temp_dir)

    def _get_classpath(self):
        return [f"{self._name}.jar"]

    def _run_jhaskell_compiler(self):
        original_dir = pathlib.Path.cwd()
        # Build the source program
        args = (
            [
                "compiler-exe",
                "--build-dir",
                f"{self._temp_dir / 'out'}",
                "--output-jar",
                f"{self._temp_dir / self._name}.jar",
                "--output-class",
                self._class_name,
                "--runtime-file-dir",
                str(original_dir.parent / "runtime"),
            ]
            + self._compiler_args
            + [f"programs/{self._package_name}.hs"]
        )
        try:
            subprocess.check_output(args).decode()
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise