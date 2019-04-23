import os

import results


class Benchmark:
    def __init__(self, name):
        self._name = name
        self._results = {}

    @property
    def name(self):
        return self._name

    @property
    def description(self):
        return self.name

    def run(self):
        self._run()
        self._write_results()

    def _run(self):
        raise NotImplementedError

    def _write_results(self):
        os.makedirs("results", exist_ok=True)
        with open("results/" + self._name, "wb") as f:
            f.write(results.show_results(self._results).encode())