package io.github.ch9;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
@Fork(value = 2, jvmArgs = {"-Xms4G", "-Xmx4G"})
public class App2 {
    /**
     * 模拟一个比较耗时的操作
     *
     * @param n
     * @return
     */
    public int doubleIt(int n) {
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return n * 2;
    }

    @Benchmark
    public int doubleAndSumSequential() {
        return IntStream.of(3, 1, 4, 1, 5, 9)
            .map(this::doubleIt)
            .sum();
    }

    @Benchmark
    public int doubleAndSumParallel() {
        return IntStream.of(3, 1, 4, 1, 5, 9)
            .parallel() // 使用并行流（12 core 机器上，6个操作能同时进行）
            .map(this::doubleIt)
            .sum();
    }

    private static final int N = 1000_0000; // 1kw

    @Benchmark
    public long iterativeSum() {
        long result = 0;
        for (long i = 1L; i <= N; i++) {
            result += i;
        }
        return result;
    }

    /**
     * @see this.parallelStreamSum
     * 1. 装箱，拆箱操作引入了不少开销
     * 2. iterate 方法产生的数据集合不容易分解，速度问题更加突出
     *
     * LongStream.rangeClosed 方法则块非常多
     */
    @Benchmark
    public long sequentialStreamSum() {
        return Stream.iterate(1L, i -> i + 1)
            .limit(N)
            .reduce(0L, Long::sum);
    }

    @Benchmark
    public long parallelStreamSum() {
        return Stream.iterate(1L, i -> i + 1)
            .limit(N)
            .parallel()
            .reduce(0L, Long::sum);
    }

    @Benchmark
    public long sequentialLongStreamSum() {
        return LongStream.rangeClosed(1,N)
            .sum();
    }

    @Benchmark
    public long parallelLongStreamSum() {
        return LongStream.rangeClosed(1,N)
            .parallel()
            .sum();
    }

    public static void main(String[] args) throws RunnerException {
        Options options = new OptionsBuilder()
            .include(App2.class.getName())
            .result("benchmark.csv")
            .resultFormat(ResultFormatType.CSV)
            .build();
        new Runner(options).run();

//        Benchmark                     Mode  Cnt    Score   Error  Units
//        App2.doubleAndSumParallel     avgt   10  107.695 ± 1.327  ms/op
//        App2.doubleAndSumSequential   avgt   10  644.224 ± 7.914  ms/op
//        App2.iterativeSum             avgt   10    2.653 ± 0.007  ms/op
//        App2.parallelLongStreamSum    avgt   10    0.293 ± 0.002  ms/op
//        App2.parallelStreamSum        avgt   10  110.458 ± 6.118  ms/op
//        App2.sequentialLongStreamSum  avgt   10    2.671 ± 0.014  ms/op
//        App2.sequentialStreamSum      avgt   10   83.681 ± 3.150  ms/op
    }
}
