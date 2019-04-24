import os
import string
import subprocess
import shutil
import pathlib
import results

import benchmark


class JMHBenchmark(benchmark.Benchmark):
    def __init__(self, name, package_name, class_name):
        super().__init__(name)
        self._package_name = package_name
        self._class_name = class_name

    def _post_compile(self):
        self._write_benchmark_main()
        self._build_benchmark()

    def _run(self):
        self._execute_bench()

    def _write_benchmark_main(self):
        temp_benchmark_package_dir = self._temp_dir / "benchmark"
        temp_benchmark_main = temp_benchmark_package_dir / "Main.java"
        substitutions = {"imports": self._get_import(), "benchmark_functions": self._get_bench_function()}

        # Write the substituted template from the template file to the temp directory
        os.makedirs(temp_benchmark_package_dir, exist_ok=True)
        with open("Main_Template.java", "rb") as input_template_file:
            template = string.Template(input_template_file.read().decode())
        output = template.substitute(substitutions)
        with temp_benchmark_main.open("wb") as output_template_file:
            output_template_file.write(output.encode())

    def _get_import(self):
        return f"import {self._package_name}.{self._class_name};"

    def _get_bench_function(self):
        # Double braces escapes to a single brace
        return f"""@Benchmark
    @BenchmarkMode(Mode.SampleTime)
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    public void {self._name}() {{
        {self._package_name}.{self._class_name}.main(args);
    }}"""

    def _build_benchmark(self):
        # Build the benchmark program
        original_dir = pathlib.Path.cwd()
        os.chdir(self._temp_dir)
        args = ["javac", "-cp", ":".join(self._get_classpath() + [str(original_dir / "deps/*")]), "benchmark/Main.java"]

        try:
            subprocess.check_output(args).decode()
            os.makedirs("META-INF", exist_ok=True)
            shutil.copyfile("BenchmarkList", "META-INF/BenchmarkList")
            shutil.copyfile("CompilerHints", "META-INF/CompilerHints")
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise
        finally:
            os.chdir(original_dir)

    def _get_classpath(self):
        raise NotImplementedError

    def _execute_bench(self):
        original_dir = pathlib.Path.cwd()
        os.chdir(self._temp_dir)
        try:
            args = [
                "java",
                "-noverify",
                "-cp",
                ":".join(self._get_classpath() + [".", str(original_dir / "deps/*")]),
                "benchmark.Main",
            ]
            output = subprocess.check_output(args)
            self._results.update(results.parse_results(output.decode()))
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise
        finally:
            os.chdir(original_dir)