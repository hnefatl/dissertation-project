import os
import shutil
import subprocess
import string
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

    def _compile(self):
        self._run_jhaskell_compiler()

    def _get_classpath(self):
        return [f"{self._name}.jar"]

    def _run_jhaskell_compiler(self):
        original_dir = pathlib.Path.cwd()
        output_jar = f"{self._temp_dir / self._name}.jar"
        # Build the source program
        args = (
            [
                "compiler-exe",
                "--build-dir",
                f"{self._temp_dir / 'out'}",
                "--output-jar",
                output_jar,
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
            self._results["size"] = str(os.path.getsize(output_jar))
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise