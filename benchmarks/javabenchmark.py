import pathlib
import shutil
import os
import os.path
import subprocess

import jmhbenchmark


class JavaBenchmark(jmhbenchmark.JMHBenchmark):
    def __init__(self, name, package, source_path):
        super().__init__(name, package, package.capitalize())
        
        self._source_path = pathlib.Path(source_path)

    def _compile(self):
        self._compile_source()

    def _post_compile(self):
        # Record the compiled file size
        compiled_file = (pathlib.Path(self._package_name) / self._class_name).with_suffix(".class")
        self._results["size"] = os.path.getsize(compiled_file)
        return super()._post_compile()

    def _get_classpath(self):
        return ["."]

    def _compile_source(self):
        original_dir = pathlib.Path.cwd()
        os.chdir(self._temp_dir)
        try:
            os.makedirs(self._package_name, exist_ok=True)
            dest_file = (pathlib.Path(self._package_name) / self._class_name).with_suffix(".java")
            shutil.copyfile(original_dir / self._source_path, dest_file)
            subprocess.check_output(["javac", dest_file])
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise
        finally:
            os.chdir(original_dir)
