import os
import shutil
import subprocess
import re
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

    def get_run_args(self):
        return ["-jar", f"{self._name}.jar"]

    def _compile(self):
        self._run_jhaskell_compiler()

    def _post_compile(self):
        self._results["size"] = jmhbenchmark.get_jar_entry_size(
            self._output_jar,
            [
                f"{self._package_name}/{s}.class"
                for s in [self._class_name, "Data", "Function", "BoxedData", "HeapObject"]
            ],
        )
        return super()._post_compile()

    def _get_classpath(self):
        return [f"{self._name}.jar"]

    def _run_jhaskell_compiler(self, extra_args=None):
        if extra_args is None:
            extra_args = []

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
            + extra_args
            + [f"programs/{self._package_name}.hs"]
        )
        try:
            return subprocess.check_output(args)
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise

    # For JHaskell, time for each stage of the compiler
    def _benchmark_compilation(self, iterations=50):
        number = 1

        # Record the output of each invocation
        outputs = []

        def bench_func():
            outputs.append(self._run_jhaskell_compiler(["--time-stages"]).decode())

        overall_times = timeit.repeat(stmt=bench_func, setup=self._pre_compile, number=number, repeat=iterations)

        time_data = []
        data_extractor = re.compile(r"(.+): (.+)ms")
        for output, overall_time in zip(outputs, overall_times):
            cumulative_time = 0
            this_run_data = []
            for line in output.splitlines():
                match = data_extractor.fullmatch(line)
                if match is None:
                    raise RuntimeError("Invalid line from compiler: " + line)
                this_time = float(match.group(2))
                this_run_data.append((match.group(1), this_time))
                cumulative_time += this_time
            #this_run_data.append(("Other", overall_time * 1000 - cumulative_time))
            time_data.append(this_run_data)
        self._results["times"] = time_data
