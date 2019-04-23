import os

class Benchmark:
    def __init__(self, name):
        self._name = name

    @property
    def name(self):
        return self._name

    @property
    def description(self):
        return self.name

    def run(self):
        raise NotImplementedError

    def _write_output(self, output):
        os.makedirs("results", exist_ok=True)
        with open("results/" + self._name, "wb") as f:
            f.write(output)