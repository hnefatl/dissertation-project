package benchmark;

import java.util.ArrayList;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.results.RunResult;

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
    public void fibonacci() {
        Output.main(new String[]{});
    }
}