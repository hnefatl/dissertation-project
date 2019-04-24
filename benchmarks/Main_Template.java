package benchmark;

import java.util.concurrent.TimeUnit;
import java.util.ArrayList;
import java.io.PrintStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import org.openjdk.jmh.util.Statistics;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.VerboseMode;
import org.openjdk.jmh.results.Result;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.results.BenchmarkResult;
import org.openjdk.jmh.results.IterationResult;

${imports}

public class Main {
    private static String[] args = new String[]{};

    public static void main(String[] args) {

        Options opt = new OptionsBuilder()
            .include(Main.class.getSimpleName())
            .verbosity(VerboseMode.SILENT)
            .forks(1)
            .build();

        try {
            // Prevent outputting all the benchmark prints
            PrintStream stdout = System.out;
            System.setOut(new PrintStream(new FileOutputStream("/dev/null")));
            RunResult run = new ArrayList<>(new Runner(opt).run()).get(0);
            System.setOut(stdout);

            BenchmarkResult bench = new ArrayList<>(run.getBenchmarkResults()).get(0);
            Statistics stats = bench.getPrimaryResult().getStatistics();
            // Also stats.getHistogram
            // http://javadox.com/org.openjdk.jmh/jmh-core/1.12/org/openjdk/jmh/util/Statistics.html

            // Display the 25th, 50th, 75th percentiles
            System.out.print("{");
            System.out.print("\"lower_quartile\": " + stats.getPercentile(25.0) + ",");
            System.out.print("\"mid_quartile\": " + stats.getPercentile(50.0) + ",");
            System.out.print("\"upper_quartile\": " + stats.getPercentile(75.0));
            System.out.println("}");
        }
        catch (RunnerException e) {
            e.printStackTrace();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
            
    ${benchmark_functions}
}
