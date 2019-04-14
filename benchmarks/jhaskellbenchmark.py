import os
import shutil
import subprocess
import string
import contextlib
import tempfile
import pathlib

import benchmark

class JHaskellBenchmark(benchmark.Benchmark):
    def __init__(self, name, source_path):
        self._name = name
        self._source_path = pathlib.Path(source_path)
        self._package_name = self._source_path.stem.lower()
        self._class_name = self._package_name.capitalize()
        self._exitstack = contextlib.ExitStack()
        self._temp_dir = None

    def __enter__(self):
        self._temp_dir = pathlib.Path(self._exitstack.enter_context(tempfile.TemporaryDirectory()))
        return self

    def __exit__(self, *args):
        self._exitstack.close()

    def run(self):
        self._compile()
        self._execute_bench()

    def _compile(self):
        self._write_benchmark_main()
        self._run_compiler()

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

    def _run_compiler(self):
        original_dir = pathlib.Path.cwd()
        # Build the source program
        args = [
            "compiler-exe",
            "--build-dir",
            f"{self._temp_dir / 'out'}",
            "--output-jar",
            f"{self._temp_dir / self._name}.jar",
            "--output-class",
            self._class_name,
            "--runtime-file-dir",
            str(original_dir.parent / "runtime"),
            f"programs/{self._package_name}.hs",
        ]
        try:
            subprocess.check_output(args).decode()
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise

        # Build the benchmark program
        os.chdir(self._temp_dir)
        args = [
            "javac",
            "-cp",
            f"{self._name}.jar:{original_dir / 'deps/*'}",
            "benchmark/Main.java"
        ]
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

    def _execute_bench(self):
        original_dir = pathlib.Path.cwd()
        os.chdir(self._temp_dir)
        try:
            args = [
                "java",
                "-noverify",
                "-cp",
                f"{self._name}.jar:.:{original_dir / 'deps/*'}",
                "benchmark.Main"
            ]
            output = subprocess.check_output(args)
        except subprocess.CalledProcessError as e:
            print(e.stdout.decode())
            raise
        finally:
            os.chdir(original_dir)

        os.makedirs("results", exist_ok=True)
        with open("results/" + self._name, "wb") as f:
            f.write(output)