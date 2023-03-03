package io.github.ch3;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class App {

    public static Integer delay(Integer n) {
        try {
            Thread.sleep((long) (Math.random() * 500));
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return n;
    }

    public static void main(String[] args) {
        // 无限的有序流
        List<BigDecimal> nums = Stream.iterate(BigDecimal.ONE, n -> n.add(BigDecimal.ONE))
            .limit(10)
            .collect(Collectors.toList());
        System.out.println(nums);
        System.out.println("------------------");

        Stream.iterate(LocalDate.now(), ld -> ld.plusDays(1L))
            .limit(10)
            .forEach(System.out::println);
        System.out.println("------------------");

        // 多次调用 Supplier 产生一个顺序的无序流
        Stream.generate(Math::random)
            .limit(10)
            .forEach(System.out::println);
        System.out.println("------------------");

        List<Integer> ints = IntStream.rangeClosed(10, 15)
            .boxed()
            .collect(Collectors.toList());
        System.out.println(ints);
        System.out.println("------------------");

        // 装配流
//        编译无法通过
//        List<Integer> integers = IntStream.of(1,2,3,4,5).collect(Collectors.toList());
//        解决方案1：使用 boxed 方法
//        List<Integer> integers = IntStream.of(1, 2, 3, 4, 5)
//            .boxed()
//            .collect(Collectors.toList());
//        解决方案2：使用 mapToObj 方法
//        List<Integer> integers = IntStream.of(1, 2, 3, 4, 5)
//            .mapToObj(Integer::valueOf)
//            .collect(Collectors.toList());
//        解决方案3：collect 方法的三参数形式
        List<Integer> integers = IntStream.of(1, 2, 3, 4, 5)
            .collect(ArrayList::new, ArrayList::add, ArrayList::addAll);
        System.out.println(integers);
        int[] intArray = IntStream.of(1, 2, 3, 4, 5).toArray();
        System.out.println(Arrays.toString(intArray));
        System.out.println("------------------");

        // reducer
        String s = Stream.of("this", "is", "a", "list")
            .collect(StringBuilder::new,
                StringBuilder::append,
                StringBuilder::append)
            .toString();
        System.out.println(s);
        System.out.println("------------------");

        // 利用 peek 对流进行调试
        int sum = IntStream.range(0, 10)
            .peek(n -> System.out.println("original: " + n))
            .map(n -> n * 2)
            .peek(n -> System.out.println("map: " + n))
            .filter(n -> n % 3 == 0)
            .peek(n -> System.out.println("filter: " + n))
            .sum();
        System.out.println(sum);
        System.out.println("------------------");

        // 获取元素数量
        // 对根据长度划分的字符串进行计数
        Map<Boolean, Long> numberLengthMap = Stream.of("this", "is", "a", "list")
            .collect(
                Collectors.partitioningBy(
                    ss -> ss.length() % 2 == 0,
                    Collectors.counting()));
        numberLengthMap.forEach((k, v) -> System.out.printf("%5s: %d%n", k, v));
        System.out.println("------------------");

        // 汇总统计(统计数据流中元素的数量、总和、最小值、最大值、平均值)
        DoubleSummaryStatistics stats = DoubleStream.generate(Math::random)
            .limit(1_000_000)
            .summaryStatistics();
        System.out.println(stats);
        System.out.println("------------------");

        // 查找流中的第一个元素
        Optional<Integer> any = Stream.of(3, 1, 4, 1, 5, 9, 2, 6, 5)
            .unordered() // 顺序并不重要
            .parallel() // 在并行流中采用 fork/join 线程池
            .map(App::delay) // 随机延迟
            .findAny(); // 返回第一个元素
        Optional<Integer> any2 = Stream.of(3, 1, 4, 1, 5, 9, 2, 6, 5)
            .unordered() // 顺序并不重要
            .map(App::delay) // 随机延迟
            .findAny(); // 返回第一个元素
        System.out.println("Any(并行流): " + any); // 不确定的值（启动多个线程，线程数取决于机器的核数，找到一个元素后关闭所有线程，访问了大量不需要的元素）
        System.out.println("Any2（顺序流）: " + any2); // 始终是 3
        // 如果流具有出现顺序，findFirst 总是返回同一个值；findAny 可以返回任意元素，适合在并行操作中使用
        System.out.println("------------------");

        // 惰性流
        OptionalInt firstEvenDoubleDivBy3 = IntStream.range(100, 200)
            .map(n -> n * 2)
            .filter(n -> n % 3 == 0)
            .findFirst();
        System.out.println(firstEvenDoubleDivBy3);
        IntStream.range(100, 200)
            .map(n -> {
                System.out.println("multi by 2: " + n);
                return n * 2;
            })
            .filter(n -> {
                System.out.println("div by 3: " + n);
                return n % 3 == 0;
            })
            .findFirst();
        System.out.println("------------------");
    }
}
