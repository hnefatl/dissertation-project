package benchmark;

import java.util.ArrayList;
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

import fib.Fib;

public class Main {
    public static void main(String[] args) {
        Options opt = new OptionsBuilder()
            .include(Main.class.getSimpleName())
            .forks(1)
            .build();

        try {
            ArrayList<RunResult> results = new ArrayList<>(new Runner(opt).run());

            System.out.println(results.size());
            for (RunResult result : results) {
                System.out.println(result.toString());
            }
        }
        catch (RunnerException e) {
            e.printStackTrace();
        }
    }

    @Benchmark
    @BenchmarkMode(Mode.SampleTime)
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    public void fibonacci() {
        fib.Fib.main(new String[]{});
    }
}