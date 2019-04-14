package benchmark;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.results.RunResult;

${imports}

public class Main {
    private static String[] args = new String[]{};

    public static void main(String[] args) {
        Options opt = new OptionsBuilder()
            .include(Main.class.getSimpleName())
            .forks(1)
            .build();

        try {
            new Runner(opt).run();
        }
        catch (RunnerException e) {
            e.printStackTrace();
        }
    }
            
    ${benchmark_functions}
}
