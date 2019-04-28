import os
import shutil
import subprocess
import string
import pathlib

import jmhbenchmark


class EtaBenchmark(jmhbenchmark.JMHBenchmark):
    def __init__(self, name, source_path, compiler_args=None):
        if compiler_args is None:
            compiler_args = []
        source_path = pathlib.Path(source_path)

        super().__init__(name, "eta", "main")
        self._source_path = source_path
        self._compiler_args = compiler_args.copy()
        self._output_jar = None

    def __enter__(self, *args):
        ret = super().__enter__(*args)
        self._output_jar = self._temp_dir / f"Run{self._source_path.stem}.jar"
        return ret

    def _compile(self):
        self._run_eta_compiler()

    def _post_compile(self):
        # Measure the compiled size
        classes = ["main", "eta"]
        self._results["size"] = jmhbenchmark.get_jar_entry_size(self._output_jar, classes)
        super()._post_compile()

    def _get_classpath(self):
        return [str(self._output_jar)]

    def _run_eta_compiler(self):
        original_dir = pathlib.Path.cwd()
        # Build the source program
        os.chdir(self._temp_dir)
        # Copy the source file to the temp dir
        source_name = self._source_path.with_suffix(".hs").name
        shutil.copyfile(original_dir / self._source_path, source_name)
        args = ["eta"] + self._compiler_args + [source_name]
        try:
            subprocess.check_output(args, stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise
        finally:
            os.chdir(original_dir)
