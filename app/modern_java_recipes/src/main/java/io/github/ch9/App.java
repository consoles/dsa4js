package io.github.ch9;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class App {
    public static void main(String[] args) {
        System.out.println("并行流和顺序流的切换（与预期结果不同）");
        List<Integer> nums = Arrays.asList(3, 1, 4, 1, 5, 9)
            .parallelStream()
            .map(n -> n * 2)
            .peek(n -> System.out.printf("%s processing %d%n", Thread.currentThread().getName(), n))
            .sequential()
            .sorted()
            .collect(Collectors.toList());
        System.out.println(nums);
//        main processing 6
//        main processing 2
//        main processing 8
//        main processing 2
//        main processing 10
//        main processing 18
//        [2, 2, 6, 8, 10, 18]
    }
}
