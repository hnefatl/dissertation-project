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
        self._compiler_args = compiler_args
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
        original_dir = pathlib.Path.cwd()
        runtime_path = "frege/run8"
        # Extract the java 8 runtime files
        os.chdir(self._temp_dir)
        args = ["jar", "xf", self._frege_jar_path, runtime_path]
        try:
            subprocess.check_output(args, stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise
        finally:
            os.chdir(original_dir)
        run_dir = self._temp_dir / runtime_path
        return sum(os.path.getsize(run_dir / f) for f in os.listdir(run_dir))
