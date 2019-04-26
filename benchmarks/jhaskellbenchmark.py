import os
import shutil
import subprocess
import string
import pathlib
import timeit

import jmhbenchmark


class JHaskellBenchmark(jmhbenchmark.JMHBenchmark):
    def __init__(self, name, source_path, compiler_args=None):
        if compiler_args is None:
            compiler_args = []
        source_path = pathlib.Path(source_path)

        super().__init__(name, source_path.stem.lower(), source_path.stem.capitalize())
        self._source_path = source_path
        self._compiler_args = compiler_args.copy()

    def __enter__(self):
        ret = super().__enter__()
        self._output_jar = (self._temp_dir / self._name).with_suffix(".jar")
        return ret

    def _compile(self):
        self._run_jhaskell_compiler()

    def _post_compile(self):
        self._results["size"] = os.path.getsize(self._output_jar)
        return super()._post_compile()

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
                str(self._output_jar),
                "--output-class",
                self._class_name,
                "--runtime-file-dir",
                str(original_dir.parent / "runtime"),
            ]
            + self._compiler_args
            + [f"programs/{self._package_name}.hs"]
        )
        try:
            subprocess.check_output(args)
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise

    # For JHaskell, time both writing jar and without
    def _benchmark_compilation(self, iterations=10):
        # Time with writing the jar
        original_args = self._compiler_args.copy()
        number = 1
        times = timeit.repeat(stmt=self._compile, setup=self._pre_compile, number=number, repeat=iterations)
        self._results["times"] = [1000 * t / number for t in times]
        # Time without writing the jar
        self._compiler_args.append("-j")
        times = timeit.repeat(stmt=self._compile, setup=self._pre_compile, number=number, repeat=iterations)
        self._results["times_no_jar"] = [1000 * t / number for t in times]
        # Reset args
        self._compiler_args = original_args