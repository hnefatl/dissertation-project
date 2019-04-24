import os
import contextlib
import tempfile
import pathlib
import timeit

import results


class Benchmark:
    def __init__(self, name):
        self._name = name
        self._results = {}
        self._exitstack = contextlib.ExitStack()
        self._temp_dir = None

    def __enter__(self):
        self._temp_dir = pathlib.Path(self._exitstack.enter_context(tempfile.TemporaryDirectory()))
        return self

    def __exit__(self, *_):
        self._exitstack.close()

    @property
    def name(self):
        return self._name

    @property
    def description(self):
        return f"{self.name}: ({self._temp_dir})"

    def execute(self):
        self._benchmark_compilation()
        self._compile()
        self._post_compile()
        self._run()
        self._write_results()

    def _compile(self):
        pass

    def _post_compile(self):
        pass

    def _run(self):
        raise NotImplementedError

    def _write_results(self):
        os.makedirs("results", exist_ok=True)
        with open("results/" + self._name, "wb") as f:
            f.write(results.show_results(self._results).encode())

    def _benchmark_compilation(self, iterations=10):
        number = 1
        times = timeit.repeat(stmt=self._compile, number=number, repeat=iterations)
        self._results["times"] = [t / number for t in times]