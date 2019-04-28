import os
import shutil
import subprocess
import string
import pathlib

import jmhbenchmark


class FregeBenchmark(jmhbenchmark.JMHBenchmark):
    def __init__(self, name, source_path, compiler_args=None):
        if compiler_args is None:
            compiler_args = []
        source_path = pathlib.Path(source_path)

        super().__init__(name, "frege.bench", "Prog")
        self._source_path = source_path
        self._compiler_args = compiler_args.copy()
        self._frege_jar_path = pathlib.Path.cwd() / "fregec.jar"

    def _compile(self):
        self._run_frege_compiler()

    def _post_compile(self):
        # Measure the compiled size
        output_class = str(
            (self._temp_dir / self._package_name.replace(".", "/") / self._class_name).with_suffix(".class")
        )
        class_size = os.path.getsize(output_class)
        runtime_files_size = self._get_frege_runtime_size()
        self._results["size"] = runtime_files_size + class_size
        super()._post_compile()

    def _get_classpath(self):
        return [str(self._frege_jar_path), str(self._temp_dir)]

    def _run_frege_compiler(self):
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
            subprocess.check_output(args, stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise

    def _get_frege_runtime_size(self):
        return jmhbenchmark.get_jar_entry_size(self._frege_jar_path, runtime_path)
