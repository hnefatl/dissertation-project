package benchmarks;

import java.util.Map;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

public class Main {
    public static void main(String[] args) {
        Options opt = new OptionsBuilder()
            .include(Main.class.getSimpleName())
            .forks(1)
            .build();

        new Runner(opt).run();
        //Map<BenchmarkRecord, RunResult> records = new Runner(opt).run();
        //for (Map.Entry<BenchmarkRecord, RunResult> result : records.entrySet()) {
        //    Result r = result.getValue().getPrimaryResult();
        //    System.out.println("Benchmark score: "
        //      + r.getScore() + " "
        //      + r.getScoreUnit() + " over "
        //      + r.getStatistics().getN() + " iterations");
        //}
    }

    @Benchmark
    public void fibonacci() {
        Output.main(new String[]{});
    }
}