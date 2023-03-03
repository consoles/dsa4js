package io.github.ch1;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Hello world!
 */
public class App {
    public static void main(String[] args) {
        Stream.of(3, 1, 4, 5, 9).forEach(x -> System.out.println(x));
        Stream.of(3, 1, 4, 5, 9).forEach(System.out::println);
        Consumer<Integer> printer = System.out::println;
        Stream.of(3, 1, 4, 5, 9).forEach(printer);
        System.out.println("--------------------");
        // Stream.generate 产生一个无限流
        Stream.generate(Math::random).limit(10).forEach(System.out::println);

        List<String> strings = Arrays.asList("this", "is", "a", "list", "of", "strings");
        List<String> sorted = strings.stream()
//            .sorted(String::compareTo)
            .sorted(Comparator.reverseOrder())
            .collect(Collectors.toList());
        System.out.println(sorted);
    }
}
